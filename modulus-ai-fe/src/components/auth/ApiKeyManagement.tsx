import React, { useState } from 'react';
import { Key, Plus, Eye, EyeOff, Save, Edit3, X, Check } from 'lucide-react';
import { apiService } from '../../services/api.service';
import { useApiKeys } from '../../contexts/ApiKeyContext';
import type { ApiKeyRead, AddApiKeyRequest } from '../../types';

interface ModelProvider {
  isApiFieldRequired: boolean;
  modelList: string[];
  providerName: string;
}

interface Props {
  providers: ModelProvider[];
}

const ApiKeyManagement: React.FC<Props> = ({ providers }) => {
  const { apiKeys, loadApiKeys, loading } = useApiKeys();
  const [showAddForm, setShowAddForm] = useState(false);
  const [newApiKey, setNewApiKey] = useState<AddApiKeyRequest>({
    providerName: '',
    keyVal: '',
  });
  const [visibleKeys, setVisibleKeys] = useState<Set<number>>(new Set());
  const [editingKey, setEditingKey] = useState<number | null>(null);
  const [editKeyValue, setEditKeyValue] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [saving, setSaving] = useState(false);

  // Remove the old loadApiKeys function since we're using the context now

  const handleAddApiKey = async () => {
    if (!newApiKey.providerName || !newApiKey.keyVal) {
      setError('Please fill in all fields');
      return;
    }

    try {
      setSaving(true);
      setError(null);
      await apiService.addApiKey(newApiKey);
      await loadApiKeys(); // Refresh the context
      setNewApiKey({ providerName: '', keyVal: '' });
      setShowAddForm(false);
    } catch (err) {
      console.error('Failed to add API key:', err);
      setError('Failed to add API key');
    } finally {
      setSaving(false);
    }
  };


  const toggleKeyVisibility = (apiKeyId: number) => {
    const newVisibleKeys = new Set(visibleKeys);
    if (newVisibleKeys.has(apiKeyId)) {
      newVisibleKeys.delete(apiKeyId);
    } else {
      newVisibleKeys.add(apiKeyId);
    }
    setVisibleKeys(newVisibleKeys);
  };

  const startEditingKey = (apiKey: ApiKeyRead) => {
    setEditingKey(apiKey.apiKeyID);
    setEditKeyValue(apiKey.apiKeyVal);
    setError(null);
  };

  const cancelEditingKey = () => {
    setEditingKey(null);
    setEditKeyValue('');
    setError(null);
  };

  const handleUpdateApiKey = async (apiKeyId: number) => {
    if (!editKeyValue.trim()) {
      setError('API key cannot be empty');
      return;
    }

    try {
      setSaving(true);
      setError(null);
      await apiService.updateApiKey(apiKeyId, { keyVal: editKeyValue });
      await loadApiKeys(); // Refresh the context
      setEditingKey(null);
      setEditKeyValue('');
    } catch (err) {
      console.error('Failed to update API key:', err);
      setError('Failed to update API key');
    } finally {
      setSaving(false);
    }
  };

  const maskApiKey = (key: string) => {
    if (key.length <= 8) return '*'.repeat(key.length);
    return key.substring(0, 4) + '*'.repeat(8) + key.substring(key.length - 4);
  };

  const availableProviders = providers.filter(p => 
    p.isApiFieldRequired && !apiKeys.some(k => k.apiKeyProviderName === p.providerName)
  );

  if (loading) {
    return (
      <div className="bg-gray-800/50 rounded-xl p-6 border border-gray-700/50">
        <div className="flex items-center gap-4">
          <div className="w-12 h-12 bg-gradient-to-br from-green-500 to-teal-600 rounded-lg flex items-center justify-center">
            <Key className="text-white" size={20} />
          </div>
          <div className="flex-1">
            <h3 className="text-lg font-semibold text-gray-100 mb-1">API Keys</h3>
            <p className="text-sm text-gray-400">Loading...</p>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="bg-gray-800/50 rounded-xl p-6 border border-gray-700/50">
      <div className="flex items-center justify-between mb-6">
        <div className="flex items-center gap-4">
          <div className="w-12 h-12 bg-gradient-to-br from-green-500 to-teal-600 rounded-lg flex items-center justify-center">
            <Key className="text-white" size={20} />
          </div>
          <div>
            <h3 className="text-lg font-semibold text-gray-100 mb-1">API Keys</h3>
            <p className="text-sm text-gray-400">Manage your AI provider API keys</p>
          </div>
        </div>
        {availableProviders.length > 0 && (
          <button
            onClick={() => setShowAddForm(true)}
            className="btn btn-primary px-4 py-2 text-sm flex items-center gap-2"
          >
            <Plus size={16} />
            Add Key
          </button>
        )}
      </div>

      {error && (
        <div className="mb-4 p-3 bg-red-900/30 border border-red-500/30 rounded-lg text-red-400 text-sm">
          {error}
        </div>
      )}

      {showAddForm && (
        <div className="mb-6 p-4 bg-gray-700/50 rounded-lg border border-gray-600/50">
          <h4 className="text-sm font-semibold text-gray-200 mb-3">Add New API Key</h4>
          <div className="space-y-3">
            <div>
              <label className="block text-xs font-medium text-gray-400 mb-1">Provider</label>
              <select
                value={newApiKey.providerName}
                onChange={(e) => setNewApiKey({ ...newApiKey, providerName: e.target.value })}
                className="w-full input px-3 py-2 text-sm border-2 border-gray-600 focus:border-blue-400 focus:ring-2 focus:ring-blue-900/20 transition-all duration-200"
              >
                <option value="">Select Provider</option>
                {availableProviders.map((provider) => (
                  <option key={provider.providerName} value={provider.providerName}>
                    {provider.providerName}
                  </option>
                ))}
              </select>
            </div>
            <div>
              <label className="block text-xs font-medium text-gray-400 mb-1">API Key</label>
              <input
                type="password"
                value={newApiKey.keyVal}
                onChange={(e) => setNewApiKey({ ...newApiKey, keyVal: e.target.value })}
                placeholder="Enter your API key"
                className="w-full input px-3 py-2 text-sm border-2 border-gray-600 focus:border-blue-400 focus:ring-2 focus:ring-blue-900/20 transition-all duration-200"
              />
            </div>
            <div className="flex gap-2">
              <button
                onClick={handleAddApiKey}
                disabled={saving}
                className="btn btn-primary px-4 py-2 text-sm flex items-center gap-2 disabled:opacity-50"
              >
                <Save size={16} />
                {saving ? 'Saving...' : 'Save'}
              </button>
              <button
                onClick={() => {
                  setShowAddForm(false);
                  setNewApiKey({ providerName: '', keyVal: '' });
                  setError(null);
                }}
                className="btn btn-secondary px-4 py-2 text-sm"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      )}

      {apiKeys.length === 0 ? (
        <div className="text-center py-8">
          <Key className="w-12 h-12 text-gray-500 mx-auto mb-3" />
          <p className="text-gray-400 text-sm">No API keys configured</p>
          {availableProviders.length > 0 && (
            <p className="text-gray-500 text-xs mt-1">Add your first API key to get started</p>
          )}
        </div>
      ) : (
        <div className="space-y-3">
          {apiKeys.map((apiKey) => (
            <div
              key={apiKey.apiKeyID}
              className="flex items-center justify-between p-4 bg-gray-700/30 rounded-lg border border-gray-600/30"
            >
              <div className="flex-1">
                <div className="flex items-center gap-3">
                  <span className="text-sm font-medium text-gray-200">
                    {apiKey.apiKeyProviderName}
                  </span>
                  <span className="px-2 py-1 bg-gray-600/50 text-gray-300 text-xs rounded-full">
                    {new Date(apiKey.apiKeyCreatedAt).toLocaleDateString()}
                  </span>
                </div>
                <div className="mt-1 flex items-center gap-2">
                  {editingKey === apiKey.apiKeyID ? (
                    <input
                      type="text"
                      value={editKeyValue}
                      onChange={(e) => setEditKeyValue(e.target.value)}
                      onKeyDown={(e) => {
                        if (e.key === 'Enter') {
                          handleUpdateApiKey(apiKey.apiKeyID);
                        } else if (e.key === 'Escape') {
                          cancelEditingKey();
                        }
                      }}
                      className="flex-1 input px-2 py-1 text-xs border border-gray-600 focus:border-blue-400 focus:ring-1 focus:ring-blue-900/20 transition-all duration-200"
                      placeholder="Enter new API key"
                      autoFocus
                    />
                  ) : (
                    <code 
                      className="text-xs text-gray-400 font-mono cursor-pointer hover:text-gray-200 transition-colors"
                      onClick={() => startEditingKey(apiKey)}
                      title="Click to edit"
                    >
                      {visibleKeys.has(apiKey.apiKeyID) ? apiKey.apiKeyVal : maskApiKey(apiKey.apiKeyVal)}
                    </code>
                  )}
                </div>
              </div>
              <div className="flex items-center gap-2">
                {editingKey === apiKey.apiKeyID ? (
                  <>
                    <button
                      onClick={() => handleUpdateApiKey(apiKey.apiKeyID)}
                      disabled={saving}
                      className="p-2 text-green-400 hover:text-green-300 transition-colors disabled:opacity-50"
                      title="Save changes"
                    >
                      <Check size={16} />
                    </button>
                    <button
                      onClick={cancelEditingKey}
                      disabled={saving}
                      className="p-2 text-red-400 hover:text-red-300 transition-colors disabled:opacity-50"
                      title="Cancel editing"
                    >
                      <X size={16} />
                    </button>
                  </>
                ) : (
                  <>
                    <button
                      onClick={() => toggleKeyVisibility(apiKey.apiKeyID)}
                      className="p-2 text-gray-400 hover:text-gray-200 transition-colors"
                      title={visibleKeys.has(apiKey.apiKeyID) ? 'Hide key' : 'Show key'}
                    >
                      {visibleKeys.has(apiKey.apiKeyID) ? <EyeOff size={16} /> : <Eye size={16} />}
                    </button>
                    <button
                      onClick={() => startEditingKey(apiKey)}
                      className="p-2 text-gray-400 hover:text-gray-200 transition-colors"
                      title="Edit key"
                    >
                      <Edit3 size={16} />
                    </button>
                  </>
                )}
              </div>
            </div>
          ))}
        </div>
      )}

      {availableProviders.length === 0 && apiKeys.length > 0 && (
        <div className="mt-4 p-3 bg-blue-900/20 border border-blue-500/30 rounded-lg">
          <p className="text-blue-400 text-sm">
            All providers that require API keys have been configured.
          </p>
        </div>
      )}
    </div>
  );
};

export default ApiKeyManagement;
