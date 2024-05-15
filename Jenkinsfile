#!groovy

node('executor') {
  checkout scm

  def commitHash  = sh(returnStdout: true, script: 'git rev-parse HEAD | cut -c-7').trim()
  def imageTag = "${env.BUILD_NUMBER}-${commitHash}"

  def pennsieveNexusCreds = usernamePassword(
    credentialsId: 'pennsieve-nexus-ci-login',
    usernameVariable: 'PENNSIEVE_NEXUS_USER',
    passwordVariable: 'PENNSIEVE_NEXUS_PW'
  )

  stage('Build') {
    withCredentials([pennsieveNexusCreds]) {
      sh 'sbt clean compile'
    }
  }

  stage('Test') {
    withCredentials([pennsieveNexusCreds]) {
      try {
        sh 'sbt test'
      } finally {
        junit '**/target/test-reports/*.xml'
      }
    }
  }

  stage('Publish') {
    withCredentials([pennsieveNexusCreds]) {
      sh 'sbt clean docker'
    }

    sh "docker tag pennsieve/model-schema-service:latest pennsieve/model-schema-service:$imageTag"
    sh 'docker push pennsieve/model-schema-service:latest'
    sh "docker push pennsieve/model-schema-service:$imageTag"
  }

  if (["main"].contains(env.BRANCH_NAME)) {
    stage('Deploy') {
      build job: "service-deploy/pennsieve-non-prod/us-east-1/dev-vpc-use1/dev/model-schema-service",
      parameters: [
        string(name: 'IMAGE_TAG', value: imageTag),
        string(name: 'TERRAFORM_ACTION', value: 'apply')
      ]
    }
  }
}
