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
  