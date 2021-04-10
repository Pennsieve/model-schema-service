data "aws_caller_identity" "current" {}

data "aws_region" "current_region" {}

# Import Model Service Data
data "terraform_remote_state" "model_service" {
  backend = "s3"

  config = {
    bucket = "${var.aws_account}-terraform-state"
    key    = "aws/${data.aws_region.current_region.name}/${var.vpc_name}/${var.environment_name}/model-service/terraform.tfstate"
    region = "us-east-1"
  }
}

# Import API Data
data "terraform_remote_state" "api" {
  backend = "s3"

  config = {
    bucket = "${var.aws_account}-terraform-state"
    key    = "aws/${data.aws_region.current_region.name}/${var.vpc_name}/${var.environment_name}/api/terraform.tfstate"
    region = "us-east-1"
  }
}

# Import Gateway Data
data "terraform_remote_state" "gateway" {
  backend = "s3"

  config = {
    bucket = "${var.aws_account}-terraform-state"
    key    = "aws/${data.aws_region.current_region.name}/${var.vpc_name}/${var.environment_name}/gateway/terraform.tfstate"
    region = "us-east-1"
  }
}

# Import Model Schema Postgres Data
data "terraform_remote_state" "model_schema_postgres" {
  backend = "s3"

  config = {
    bucket = "${var.aws_account}-terraform-state"
    key    = "aws/${data.aws_region.current_region.name}/${var.vpc_name}/${var.environment_name}/pennsieve-postgres/terraform.tfstate"
    region = "us-east-1"
  }
}

# Import Region Data
data "terraform_remote_state" "region" {
  backend = "s3"

  config = {
    bucket = "${var.aws_account}-terraform-state"
    key    = "aws/${data.aws_region.current_region.name}/terraform.tfstate"
    region = "us-east-1"
  }
}
