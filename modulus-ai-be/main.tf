terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "6.8.0"
    }
  }
}

provider "google" {
  project = "haskread"
  region  = "europe-west1"
}

resource "google_cloud_run_v2_service" "backend" {
  name     = "modulus-ai-backend"
  location = "europe-west1"
  project  = "haskread"

  template {
    containers {
      image = var.backend_image

      resources {
        limits = {
          memory = "1Gi"
          cpu    = "2"
        }
        cpu_idle = true
        startup_cpu_boost = true
      }

      # environment variables
      env {
        name  = "POSTGRES_DB"
        value = var.postgres_db
      }
      env {
        name  = "POSTGRES_USER"
        value = var.postgres_user
      }
      env {
        name  = "POSTGRES_PASSWORD"
        value = var.postgres_password
      }
      env {
        name  = "POSTGRES_HOST"
        value = var.postgres_host
      }
      env {
        name  = "MODULUS_DB_TIMEOUT"
        value = "30"
      }
      env {
        name  = "MODULUS_APP_PORT"
        value = "8080"
      }
      env {
        name  = "MODULUS_JWT_SECRET"
        value = var.modulus_jwt_secret
      }
      env {
        name  = "MODULUS_LOG_LEVEL"
        value = "debug"
      }
      env {
        name  = "MODULUS_ENVIRONMENT"
        value = "development"
      }
      env {
        name  = "MODULUS_REDIS_URL"
        value = "redis://localhost:6379"
      }
      env {
        name  = "MODULUS_API_TIMEOUT"
        value = "60"
      }
      env {
        name  = "MODULUS_MAILGUN_API"
        value = var.modulus_mailgun_api
      }
      env {
        name  = "MODULUS_FILE_UPLOAD_PATH"
        value = "/home/appuser/app/uploads"
      }

      volume_mounts {
        name       = "gcs-1"
        mount_path = "/home/appuser/app/uploads"
      }
    }

    scaling {
      min_instance_count = 0      
    }

    volumes {
      name = "gcs-1"
      gcs {
        bucket = "modulus_ai_bucket"
        read_only = false
      }
    }
  }
}
