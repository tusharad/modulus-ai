import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import { apiService } from '../services/api.service';
import type { ApiKeyRead } from '../types';

interface ApiKeyContextType {
  apiKeys: ApiKeyRead[];
  loadApiKeys: () => Promise<void>;
  getApiKeyForProvider: (providerName: string) => string | undefined;
  loading: boolean;
}

const ApiKeyContext = createContext<ApiKeyContextType | undefined>(undefined);

export const useApiKeys = () => {
  const context = useContext(ApiKeyContext);
  if (!context) {
    throw new Error('useApiKeys must be used within an ApiKeyProvider');
  }
  return context;
};

interface ApiKeyProviderProps {
  children: ReactNode;
}

export const ApiKeyProvider: React.FC<ApiKeyProviderProps> = ({ children }) => {
  const [apiKeys, setApiKeys] = useState<ApiKeyRead[]>([]);
  const [loading, setLoading] = useState(true);

  const loadApiKeys = async () => {
    try {
      setLoading(true);
      const keys = await apiService.getApiKeys();
      setApiKeys(keys);
    } catch (err) {
      console.error('Failed to load API keys:', err);
      setApiKeys([]);
    } finally {
      setLoading(false);
    }
  };

  const getApiKeyForProvider = (providerName: string): string | undefined => {
    const apiKey = apiKeys.find(key => key.apiKeyProviderName === providerName);
    return apiKey?.apiKeyVal;
  };

  useEffect(() => {
    loadApiKeys();
  }, []);

  const value: ApiKeyContextType = {
    apiKeys,
    loadApiKeys,
    getApiKeyForProvider,
    loading,
  };

  return (
    <ApiKeyContext.Provider value={value}>
      {children}
    </ApiKeyContext.Provider>
  );
};

