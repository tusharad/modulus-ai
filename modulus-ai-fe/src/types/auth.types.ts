export interface RegisterRequest {
    registerEmail: string;
    registerPassword: string;
    registerConfirmPassword: string;
  }
  
  export interface LoginRequest {
    loginEmail: string;
    loginPassword: string;
  }
  
  export interface UserProfile {
    userProfileId: string;
    userProfileEmail: string;
  }
  
export interface AuthTokens {
  accessToken: string;
  refreshToken: string;
}

export interface ChangePasswordRequest {
  oldPassword: string;
  newPassword: string;
  confirmNewPassword: string;
}

export interface AddApiKeyRequest {
  providerName: string;
  keyVal: string;
}

export interface UpdateApiKeyRequest {
  keyVal: string;
}

export type ApiKeyID = number;

export interface ApiKeyRead {
  apiKeyID: ApiKeyID;
  apiKeyUserID: string;
  apiKeyProviderName: string;
  apiKeyVal: string;
  apiKeyCreatedAt: string;
}
  