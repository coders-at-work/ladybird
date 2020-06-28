(ns ladybird.ext.dto
    (:require ;[glp-host-app-api.infrastructure.converter :as c]
              [ladybird.data.db-converter :as dc]
              [ladybird.data.build-in-validator :as bv]
              [ladybird.ext.schema :as ladybird-schema]
              [ladybird.data.enum :as enum]
              [schema.core :as s])
    (:import org.joda.time.DateTime
             java.util.Date
             java.math.BigDecimal
             ))

(defn- schema-spec-from-validator
  [validator]
  (let [vs (if (sequential? validator) validator [validator])
        validator-schema-spec {
                               bv/not-nil {:required true}
                               bv/not-blank {:required true :schema ladybird-schema/not-blank}
                               bv/not-zero {:schema s/Num}
                               bv/is-int {:schema s/Int}
                               bv/is-int-str {:schema s/Str}
                               bv/is-non-negative-int-str {:schema s/Str}
                               bv/is-number {:schema s/Num}
                               bv/is-boolean {:schema s/Bool}
                               bv/is-boolean-str {:schema s/Str}
                               bv/is-email {:schema s/Str}
                               }
        ]
    (reduce
      (fn [spec v]
          (merge spec (validator-schema-spec v))
          )
      nil
      vs
      )))

(defn- schema-spec-from-converter
  [converter]
  (let [schema (if (enum/enum? converter)
                 (apply s/enum (enum/spec-keys converter))
                 (let [converter-schemas {
                                          dc/JAVA-TIME-INSTANT java.time.Instant
                                          dc/EDN s/Any
                                          dc/MSSQL-ROWVERSION Long
                                          ; c/JODA-TIMESTAMP DateTime
                                          dc/BOOL s/Bool
                                          dc/DATETIME Date
                                          dc/DATE Date
                                          dc/DECIMAL BigDecimal
                                          dc/STR->DATETIME s/Str
                                          dc/INT->STRING s/Int
                                          }
                       ]
                   (converter-schemas converter)
                   ))
        ]
    {:schema schema}
    )
  )

(defn- schema-spec-to-schema
  [{:keys [required schema] :as spec}]
  (if required
    schema
    (s/maybe schema)
    )
  )

(defn- schema-of-field
  ""
  [field entity-converters entity-validators]
  (let [spec (merge (some-> (get entity-converters field) schema-spec-from-converter)
                    (some-> (get entity-validators field) schema-spec-from-validator)
                    )
        schema (when spec (schema-spec-to-schema spec))
        ]
    (or schema s/Str)
    )
  )

(defn generate-schema-from-entity
  ""
  [{:keys [converters] entity-fields :fields validators :validate :as entity} {:keys [includes excludes fields-spec] :as opts}]
  (let [schema-fields (or includes
                          (some->> excludes (apply disj (set entity-fields)) seq)
                          entity-fields
                          )
        schema-infered (reduce
                         (fn [schema field]
                             (assoc schema field (schema-of-field field converters validators))
                             )
                         {}
                         schema-fields
                         )
        ]
    (merge schema-infered (select-keys fields-spec schema-fields))
    )
  )

(defmacro defdto-from-entity
  ""
  ([dto-name entity]
   `(defdto-from-entity ~dto-name ~entity nil)
   )
  ([dto-name entity opts]
   `(def ~dto-name (generate-schema-from-entity ~entity ~opts))
   )
  )
