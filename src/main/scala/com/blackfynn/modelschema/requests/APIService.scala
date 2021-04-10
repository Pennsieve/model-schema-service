package com.blackfynn.modelschema.requests.apiservice

import com.blackfynn.dtos.ModelPropertyRO
import com.blackfynn.modelschema.model.implicits._

/**
  * Represents a request to create a new dataset in the model (formerly concept) service.
  *
  * @param name
  * @param description
  * @param properties
  */
case class CreateDataset(
  name: String,
  description: Option[String],
  properties: List[ModelPropertyRO] = List.empty
)
