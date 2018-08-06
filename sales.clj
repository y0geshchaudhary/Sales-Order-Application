(ns test1.sales
  (:require [clojure.string :as str])
  (:gen-class))

(use 'clojure.java.io)

(defn read-from-file [file numberOfData]
  (let [string1 (slurp file)]
    (let [dataVector (str/split string1 #"([\|\n])")]
      (let [b (count dataVector)]
        (let [vectorSlices (partition numberOfData dataVector)]
          (let [s (into (sorted-map) (for [x vectorSlices] [(let [i (Integer/parseInt (nth x 0))] i) x]))]
            s
            )
          )
        )
      )
    )
  )

(defn displayCust [custMap]
  (doseq [x (vals custMap)]
    (println (str (nth x 0) ":[\"" (nth x 1) "\" \"" (nth x 2) "\" \"" (str/trim-newline (nth x 3)) "\"]"))
    )
  )

(defn displayProd [prodMap]
  (doseq [x (vals prodMap)]
    (println (str (nth x 0) ":[\"" (nth x 1) "\" \"" (str/trim-newline (nth x 2)) "\"]")) ; "\" \"" (str/trim-newline (nth x 2))
    )
  )

(defn displaySales [custMap prodMap salesMap]
  (doseq [x (vals salesMap)]
    (let [custId (nth (get custMap (Integer/parseInt (nth x 1))) 1)]
      (let [prodId (nth (get prodMap (Integer/parseInt (nth x 2))) 1)]
        (println (str (nth x 0) ":[\"" custId "\" \"" prodId "\" \"" (str/trim-newline (nth x 3)) "\"]"))
        ))
    )
  )

(defn custPurchase [custMap prodMap salesMap]
  (println "Insert customer name.")
  (let [custName (read-line)]
    (let [custIDMap (loop [custValues (vals custMap)]
                      (let [n (into (sorted-map) (for [cust custValues] [(str/lower-case (nth cust 1)) (nth cust 0)]))]
                        n
                        ))]
      (let [id (get custIDMap (str/lower-case (str/trim custName)))]
        (if (not= id nil)
          (do
            (let [salesIdMap (loop [salesValues (vals salesMap)]
                               (let [n (into (sorted-map) (for [sales salesValues]
                                                               (cond
                                                                 (= (nth sales 1) id) [(str/trim (nth sales 2)) (Integer/parseInt (str/trim (nth sales 3)))]
                                                                 )
                                                            ))]
                                 n
                                 ))]
              (let [itemIdList (keys salesIdMap)]
                (let [priceList (for [itemId itemIdList]
                                     (Float/parseFloat (nth (get prodMap (Integer/parseInt itemId)) 2))
                                  )]
                  (println (str (str/trim (nth (get custMap (Integer/parseInt id)) 1)) ": $" (format "%.2f" (reduce + (map * priceList (vals salesIdMap))))))
                  )
                )

              )
            )
          (println "No customer with this name.")
          )
        )
      )
    )
  )

(defn prodCount [prodMap salesMap]
  (println "Insert product name.")
  (let [prodName (read-line)]
    (let [prodIDMap (loop [prodValues (vals prodMap)]
                      (let [n (into (sorted-map) (for [prod prodValues] [(str/trim (str/lower-case (nth prod 1))) (str/trim (nth prod 0))]))]
                        n
                        ))]
      (let [id (get prodIDMap (str/lower-case (str/trim prodName)))]
        (if (not= id nil)
          (do
            (let [prodSaleList (loop [salesValues (vals salesMap)]
                                 (let [n (into (list) (for [sales salesValues]
                                              (cond
                                                (= (str/trim (nth sales 2)) id) (Integer/parseInt (str/trim (nth sales 3)))
                                                :else 0
                                                )
                                              ))]
                                    n
                                 )
                                )
                  ]
              (println (str (nth (get prodMap (Integer/parseInt id)) 1) ": " (format "%d" (reduce + prodSaleList))))
              )
            )
          (println "No product with this name.")
          )
        )
      )
    )
  )


(defn displayMenu [custMap prodMap salesMap]
  (println)
  (println "*** Sales Menu ***")
  (println "------------------")
  (println "1. Display Customer Table")
  (println "2. Display Product Table")
  (println "3. Display Sales Table")
  (println "4. Total Sales for Customer")
  (println "5. Total Count for Product")
  (println "6. Exit")
  (println "Enter an option?")
  (let [userInput (read-line)]
    (case userInput "1" (do (displayCust custMap)
                            (println)
                            (displayMenu custMap prodMap salesMap))
                    "2" (do (displayProd prodMap)
                            (println)
                            (displayMenu custMap prodMap salesMap))
                    "3" (do (displaySales custMap prodMap salesMap)
                            (println)
                            (displayMenu custMap prodMap salesMap))
                    "4" (do (custPurchase custMap prodMap salesMap)
                            (displayMenu custMap prodMap salesMap))
                    "5" (do (prodCount prodMap salesMap)
                            (displayMenu custMap prodMap salesMap))
                    "6" (do (println "Good Bye.")
                            )
                    (do (println "Incorrect choice. Choose again.")
                        (displayMenu custMap prodMap salesMap))

                    )
    )
  )

(defn -main [& args]
  (def custMap (read-from-file "cust.txt", 4))
  (def prodMap (read-from-file "prod.txt", 3))
  (def salesMap (read-from-file "sales.txt", 4))
  (displayMenu custMap prodMap salesMap)
  )

(-main)