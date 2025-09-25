variable "backend_image" {
  description = "Docker image for backend"
  type        = string
}

variable "postgres_db" {
  type      = string
  sensitive = true
}

variable "postgres_user" {
  type      = string
  sensitive = true
}

variable "postgres_password" {
  type      = string
  sensitive = true
}

variable "postgres_host" {
  type      = string
  sensitive = true
}

variable "modulus_jwt_secret" {
  type      = string
  sensitive = true
}

variable "modulus_mailgun_api" {
  type      = string
  sensitive = true
}
