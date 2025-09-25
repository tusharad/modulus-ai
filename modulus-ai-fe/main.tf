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

resource "google_cloud_run_service" "frontend" {
  name     = "modulus-ai-frontend"
  location = "europe-west1"

  template {
    spec {
      containers {
        image = var.image_name
        resources {
          limits = {
            memory = "1Gi"
            cpu    = "2"
          }
        }
      }
    }
  }

  traffic {
    percent         = 100
    latest_revision = true
  }
}
