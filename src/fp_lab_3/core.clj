(ns fp-lab-3.core
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  [["-a" "--algo ALGO" "Interpolation algorithm: linear | lagrange"
    :default "linear"
    :parse-fn str
    :validate [#(#{"linear" "lagrange"} %) "ALGO must be 'linear' or 'lagrange'"]]

   ["-s" "--step STEP" "Step size (double)"
    :default 1.0
    :parse-fn #(Double/parseDouble %)
    :validate [pos? "STEP must be positive"]]

   ["-w" "--window N" "Window size for lagrange (int >= 2). Not allowed for linear."
    :default 5
    :parse-fn #(Long/parseLong %)
    :validate [#(>= % 2) "window must be >= 2"]]

   ["-h" "--help" "Show help"]])


(defn exit-program
  ([code]
   (System/exit code))
  ([code message]
   (println message)
   (System/exit code)))

(defn validate-args! [{:keys [options errors summary]}]
  (when (seq errors)
    (exit-program 1 (str "CLI errors:\n" (str/join "\n" errors) "\n\n" summary)))

  (when (:help options)
    (exit-program 0 summary))

  ;; если выбран linear и пользователь указал окно — ошибка
  (when (and (= "linear" (:algo options))
             (some? (:window options)))
    (exit-program 1 "ERROR: --window нельзя задавать для linear-интерполяции. Окно всегда = 2."))

  options)


(defn stepped-x-values
  "Все x, кратные step, в [l, r]. Устойчива к double-погрешностям."
  [l r step]
  (let [eps (* 1e-9 step)                 ; маленький допуск
        k0 (long (Math/ceil  (/ (- l eps) step)))
        k1 (long (Math/floor (/ (+ r eps) step)))]
    (map (fn [k] (* k step))
         (range k0 (inc k1)))))


(defn my-linear-interpolation [points step l r]
  (let [[p1 p2] points
        x1 (:x p1) y1 (:y p1)
        x2 (:x p2) y2 (:y p2)
        x-values (stepped-x-values l r step)]
    (map (fn [x]
           (let [t (/ (- x x1) (- x2 x1))]
             {:x (double x)
              :y (+ y1 (* t (- y2 y1)))}))
         x-values)))

(defn lagrange-polynomial [points x]
  (let [n (count points)]
    (reduce
     (fn [acc i]
       (let [pi (nth points i)
             xi (:x pi)
             yi (:y pi)
             li (reduce
                 (fn [li j]
                   (if (not= i j)
                     (let [pj (nth points j)
                           xj (:x pj)]
                       (* li (/ (- x xj) (- xi xj))))
                     li))
                 1.0
                 (range n))]
         (+ acc (* li yi))))
     0.0
     (range n))))

(defn my-lagrange-interpolation [points step l r]
  (let [x-values (stepped-x-values l r step)]
    (map (fn [x] {:x (double x)
                  :y (lagrange-polynomial points x)})
         x-values)))

;; ----- утилиты окна -----

(defn push-window
  "Добавление нового элемента p в окно
   Если размер стал больше максимального, удаляем перый элемент"
  [window window-size p]
  (let [new-window (conj window p)]
    (if (> (count new-window) window-size)
      (subvec new-window 1)
      new-window)))

(defn mid-x
  "Возвращает точку, кратную step, ближайшую к центру [x0, x1].
   Если таких точек две (равноудалены), берёт меньшую."
  [points step]
  (let [x0 (:x (first points))
        x1 (:x (peek points))
        c  (/ (+ x0 x1) 2.0)          ; центр отрезка
        k  (Math/floor (/ c step))   ; нижний индекс кратной точки
        m0 (* k step)                ; кратная точка снизу
        m1 (+ m0 step)               ; кратная точка сверху
        d0 (Math/abs (- c m0))
        d1 (Math/abs (- c m1))]
    (if (<= d0 d1)   ; <= чтобы при равенстве выбрать меньшую
      (double m0)
      (double m1))))

;; ----- фазы -----

(defn check-order
  "Если x координата новой точки меньше предыдущей, выбросит исключение"
  [points p]
  (when-let [prev-x (some-> points peek :x)]
    (when (< (:x p) prev-x)
      (throw (ex-info "Input x is not nondecreasing"
                      {:prev-x prev-x :x (:x p) :point p})))))

(defn init-fn
  [points f-interpolate step]
  (let [x-start (:x (first points))
        x-mid   (mid-x points step)]
    (f-interpolate points x-start x-mid)))

(defn step-fn
  [old-points new-points f-interpolate step]
  (let [prev-mid (mid-x old-points step)
        new-mid  (mid-x new-points step)
        x-left   (+ prev-mid step)]
    (when (<= x-left new-mid)
      (f-interpolate new-points x-left new-mid))))

(defn final-fn
  [points f-interpolate step]
  (let [prev-mid (mid-x points step)
        x-end    (:x (peek points))
        x-left   (+ prev-mid step)]
    (when (<= x-left x-end)
      (f-interpolate points x-left x-end))))

;; ----- шаг reduce -----

(defn consumer-step
  [window-size
   f-init
   f-step
   f-validate
   {:keys [window inited?] :as state} p]

  (f-validate window p)

  (let [new-window (push-window window window-size p)
        filled?    (= (count new-window) window-size)
        new-state  (assoc state :window new-window)]
    (cond
      ;; 1) окно ещё не заполнено
      (not filled?)
      new-state

      ;; 2) первое заполнение => init
      (and filled? (not inited?))
      (do (f-init new-window)
          (assoc new-state :inited? true))

      ;; 3) steady-state => step
      :else
      (do (f-step window new-window)
          new-state))))

;; ----- потоковый ввод -----

(defn run-interpolation! [points step window-size f-interpolate emit!]
  (let [f-validate (fn [window p] (check-order window p))
        f-init     (fn [new-window] (init-fn new-window f-interpolate step))
        f-step     (fn [old-window new-window] (step-fn old-window new-window f-interpolate step))
        f-final    (fn [window] (final-fn window f-interpolate step))
        init-state  {:window [] :inited? false}
        end-state   (reduce
                     (partial consumer-step window-size
                              (fn [w] (emit! (f-init w)))
                              (fn [ow nw] (emit! (f-step ow nw)))
                              f-validate)
                     init-state points)]
    (when (:inited? end-state)
      (emit! (f-final (:window end-state))))))

(defn run-linear! [points step emit!]
  (let [window-size 2
        f-interpolate (fn [window l r] (my-linear-interpolation window step l r))]
    (run-interpolation! points step window-size f-interpolate emit!)))

(defn run-lagrange! [points step window-size emit!]
  (let [f-interpolate (fn [window l r] (my-lagrange-interpolation window step l r))]
    (run-interpolation! points step window-size f-interpolate emit!)))

(defn parse-point [line]
  (let [[xs ys] (str/split line #"\s+")]
    {:x (Double/parseDouble xs)
     :y (Double/parseDouble ys)}))

(defn stdin-points
  []
  (let [rdr (java.io.BufferedReader. *in*)]
    (->> (line-seq rdr)
         (remove clojure.string/blank?)
         (map parse-point))))

(defn -main [& args]
  (let [{:keys [options errors summary]}
        (parse-opts args cli-options)

        {:keys [algo step window]}
        (validate-args! {:options options :errors errors :summary summary})

        points (stdin-points)

        emit! (fn [pts]
                (doseq [{:keys [x y]} pts]
                  (println x y)))]

    (try
      (case algo
        "linear"
        (run-linear! points step emit!)

        "lagrange"
        (run-lagrange! points step window emit!))

      (catch clojure.lang.ExceptionInfo e
        (binding [*out* *err*]
          (println "ERROR:" (.getMessage e))
          (println "Details:" (ex-data e)))
        (System/exit 1)))))
