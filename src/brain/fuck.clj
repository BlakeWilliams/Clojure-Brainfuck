(ns brain.fuck
  (:gen-class))

(def ptr (atom 0)) ; Initialize the pointer to 0
(def cells (atom (vec (repeat 30000 0)))) ; Plenty of room!

(defn start [place commands]
  (loop [brackets 1]
   (when-not (zero? brackets)
     (swap! place inc)
     (case (nth commands @place)
       \] (recur (dec brackets))
       \[ (recur (inc brackets))
       (recur brackets)))))

(defn end [place commands]
  (loop [brackets 1]
   (when-not (zero? brackets)
     (swap! place dec)
     (case (nth commands @place)
       \] (recur (inc brackets))
       \[ (recur (dec brackets))
       (recur brackets)))))

(defn exec-command [commands place]
  (case (nth commands @place)
    \> (swap! ptr inc)
    \< (swap! ptr dec)
    \+ (reset! cells (assoc @cells @ptr (inc (get @cells @ptr))))
    \- (reset! cells (assoc @cells @ptr (dec (get @cells @ptr))))
    \. (do 
         (print (char (get @cells @ptr))) 
         (flush))
    \, (reset! cells (assoc @cells @ptr (int (char (. System/in read)))))
    \[ (if (== (get @cells @ptr) 0)
         (start place commands))
    \] (if-not (== (get @cells @ptr) 0)
         (end place commands))
    ()))

(defn interpret [commands]
  (loop [place (atom 0)]
    (exec-command commands place)
    (swap! place inc)
    (if-not (= @place (count commands))
      (recur place))))

(defn -main [& args]
  (if (nth args 0)
      (interpret (slurp (nth args 0)))
    (println "Please specify a file")))
