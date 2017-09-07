(ns logical-interpreter)

(use 'clojure.string)

(defn getVerb [line]
  (get (re-matches #"(.*?)\(.*?\).*" line) 1 nil))

(defn getParameters [line]
  (if-let [token (get (re-matches #".*?\((.*?)\).*" line) 1 nil)]
          (map trim (split token  #","))))

(defn getFactParameterComparator
  "Devuelve una función de comparación de los parámetros del fact"
  [line]
  ;(printf "Getting parameter comparator of %s\n" line)
  (let [parameters (getParameters line)]
       (fn[x] (= parameters x) ))) ; Función de igualdad de los parámetros del fact
  

(defn isFact? [line] (boolean (re-matches #".*?\([a-z, ]*?\)\." line)))
(defn isRule? [line] (boolean (re-matches #".*?\([A-Z, ]*?\) *:-.*\." line)))

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


(defn addRule [db line]
  (let [ [rule conditions] (split line #":-")]
       
      ;(printf "Separados: %s ### %s\n" rule conditions)
       
        (let [
              verb (getVerb rule)
              parameters (getParameters rule)
              ]
            
            ;(printf "Regla: %s Verbo: %s Parametros: %s Conditions: %s\n" rule verb (pr-str parameters) conditions)
         
            ; Construímos la función que evalúa la regla. Separamos los fact que componen a la condición de la regla, y a cada uno lo generamos un comparador. Finalmente, unimos a todos los comparadores con un and reduciendo.
         
            (swap! db update-in [verb] (fn[old-value] (fn
                [query-parameters]
                
                ; paramMapping permite a las condiciones buscar el valor asociado en la regla a cada parámetro.
                (let [paramMapping (zipmap parameters query-parameters)]
                     
                     ;(printf "Mapeo de parámetros: %s\n" paramMapping)
                     
                     ; Todos los fact que componen la condición deben cumplirse. Devolvemos una función que comprueba esto.
                     (every? true? 
                             (map (fn
                                    [condition] 
                                    ; Generamos la función de comparación de uno de los fact.
                                    
                                    ;(printf "Generando comparador para %s\n" (get condition 0))
                                    
                                    ;(printf "Parámetros mapeados %s\n" (pr-str (map paramMapping (map trim (split (get condition 2) #",")))))
                                    
                                    ((get @db (get condition 1)) 
                                    (map paramMapping (map trim (split (get condition 2) #","))))
                                    
                                  )
                                  (re-seq #"(\S*?)\((.*?)\)" conditions)
                              )
                      )
                )))
            )
          )
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
                                       (addRule db line)
                                       )
                      ;:else (printf "Ignoring Line\n") ; TODO lanzar error, o de alguna forma invalidar la db
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
    ;(printf "Data: %s\n" ((data query-verb) query-parameters))
    (when-let [rule-evaluator (data query-verb)]
	    (rule-evaluator query-parameters))  
  )
)
