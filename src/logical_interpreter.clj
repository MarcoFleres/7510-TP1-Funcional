(ns logical-interpreter)

(use 'clojure.string)
;(use '[clojure.pprint :as pprint])

(defn getVerb [line]
  (get (re-matches #"(.*?)\(.*?\)\.?$" line) 1 nil))

(defn getParameters [line]
  (if-let [token (get (re-matches #".*?\((.*?)\)\.?" line) 1 nil)]
          (map trim (split token  #","))))

(defn getFactParameterComparator
  "Devuelve una función de comparación de los parámetros del fact"
  [line]
  ;(printf "Getting parameter comparator of %s\n" line)
  (let [parameters (getParameters line)]
       (fn[x] (do
                ;(printf "Comparando con %s\n" (pr-str parameters))
                (= parameters x)
                )))) ; Función de igualdad de los parámetros del fact
  

(defn isFact? [line] (boolean (re-matches #".*?\(.*?\)\." line)))  ;FIXME corregir condición para diferenciarla de las reglas
(defn isRule? [line] false) ;TODO implementar

(defn combineFacts
  [newFactRules oldfactRules]
  (if (= oldfactRules nil) newFactRules
      (fn [x] (or (oldfactRules x) (newFactRules x)))))


(defn addFact [db line]
  ;Llamamos verb al nombre de la función
  ;Llamamos curCondition a la función de igualdad de los parámetros del fact
  (let [verb (getVerb line)  ;TODO asignar el verbo (con regex de la línea)
        curCondition (getFactParameterComparator line)  ;TODO asignar función que compara con los parámetros del fact
        ]
      ;(printf "Adding Fact %s: verb:%s cond: %s\n" line verb curCondition)
      
      ; Si no existe la regla, la creamos con la condición de igualdad con el Fact
      ; Si existe, agregamos el Fact a la condición con una OR.
      
      (swap! db update-in [verb] (partial combineFacts curCondition))
      ;(printf "DB: %s\n" @db)
    )
  )
  
  

(defn parseDatabase
  "Recibe un string con la definición de la base de datos, y devuelve un mapa de reglas."
  [database]
  (let [db (atom {})]
       (doseq [line (map trim (split-lines database))]
              
                ;(printf "Parsing Line \"%s\": " line )
                (cond 
                      (isFact? line) (do 
                                        ;(printf "Is Fact\n")
                                        (addFact db line))
                      (isRule? line) (do
                                       ;(printf "Is Rule\n")
                                       ;(addRule db line)
                                       )
                      ;:else (printf "Ignoring Line\n")
                )
       )
    ;(printf "DB: %s\n" @db)
    @db
  )
)
  
  

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (let [data (parseDatabase database)
        query-verb (getVerb query)
        query-parameters (getParameters query)
        ]
    ;(printf "Getting Verb: %s\n" query-verb)
    ((data query-verb) query-parameters)
  )
)



