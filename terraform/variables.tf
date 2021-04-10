variable "aws_account" {}

variable "ecs_task_iam_role_id" {}

variable "environment_name" {}

variable "service_name" {}

variable "vpc_name" {}

variable "bioportal_host" {
  default = "data.bioontology.org"
}

variable "bioportal_rate_limit" {
  default = "15"
}
