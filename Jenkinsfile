pipeline {
  agent any

  stages {
    stage('Setup') {
      steps {
        curl -sSLO 'https://get.haskellstack.org/stable/linux-x86_64.tar.gz'
        tar -xf linux-x86_64.tar.gz
        cp stack-*/stack .
        rm -rf stack-*
      }
    }
    stage('Build') {
      steps {
        sh './stack build' 
      }
    }
  }
}
