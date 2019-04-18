pipeline {
  agent {
    docker { image 'haskell:8.6.3' }
  }

  stages {
    stage('Build') {
      steps {
        sh 'stack build' 
      }
    }
  }
}
