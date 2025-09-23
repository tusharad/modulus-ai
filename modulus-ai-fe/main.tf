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
  name     = "modulus-ai-frontend"
  location = "europe-west1"

  template {
    containers {
      image = var.image
    }

    scaling {
      min_instance_count = 0
    }
  }
}
