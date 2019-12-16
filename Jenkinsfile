pipeline {
  agent any

  stages {
    stage('Setup') {
      steps {
        script {
            if (fileExists('stack')) {
                echo 'Stack already present'
            } else {
                echo 'Downloading stack tool'
                
                sh 'curl -sSLO https://get.haskellstack.org/stable/linux-x86_64.tar.gz'
                sh 'tar -xf linux-x86_64.tar.gz'
                sh 'cp stack-*/stack .'
                sh 'rm -rf stack-*'
            }
        }
      }
    }
    stage('Build') {
      steps {
        sh './stack clean'
        sh './stack build'
        sh 'printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)" > version'
      }
    }
  }
  post {
    always {
      sh 'find .stack-work/install -name "chat" -print0 | xargs -r -0 ls -t | head -n 1 | xargs -I "{}" mv "{}" chat'
      archiveArtifacts artifacts: 'chat'
      archiveArtifacts artifacts: 'version'
    }
  }
}
