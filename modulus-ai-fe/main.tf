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

resource "google_cloud_run_v2_service" "frontend" {
  name     = "modulus-ai-frontend"
  location = "europe-west1"

  template {
      containers {
        image = var.image_name
        resources {
          limits = {
            memory = "1Gi"
            cpu    = "2"
          }
          cpu_idle = true
          startup_cpu_boost = true
        }
      }
  }

  scaling {
      min_instance_count = 0
    }

}

resource "google_cloud_run_v2_service_iam_member" "member" {
  project  = "haskread"
  location = "europe-west1"
  name = google_cloud_run_v2_service.frontend.name
  role   = "roles/run.invoker"
  member = "allUsers"
}