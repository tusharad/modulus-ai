import React, { useEffect, useState } from "react";
import { apiService } from "../../services/api.service";
import { ChevronDown, LogOut, Settings, User } from "lucide-react";

interface ModelProvider {
  isApiFieldRequired: boolean;
  modelList: string[];
  providerName: string;
}

interface Props {
  onChange: (config: { provider: string; model: string; apiKey?: string }) => void;
  onLogout: () => void;
}

const TopBar: React.FC<Props> = ({ onChange, onLogout }) => {
  const [providers, setProviders] = useState<ModelProvider[]>([]);
  const [selectedProvider, setSelectedProvider] = useState<ModelProvider | null>(null);
  const [selectedModel, setSelectedModel] = useState("");
  const [apiKey, setApiKey] = useState("");
  const [dropdownOpen, setDropdownOpen] = useState(false);


  useEffect(() => {
    const loadProviders = async () => {
      try {
        const data_ = await apiService.getModelProviders();
        const data = data_.filter((p) => p.modelList.length > 0);
        setProviders(data);
        if (data.length > 0) {
          const firstWithModels = data.find(p => p.modelList.length > 0);
          if (firstWithModels) {
            setSelectedProvider(firstWithModels);
            setSelectedModel(firstWithModels.modelList[0] || "");
          } else {
            setSelectedProvider(data[0]);
            setSelectedModel("");
          }
        }
      } catch (err) {
        console.error("Failed to load model providers", err);
      }
    };
    loadProviders();
  }, []);

  useEffect(() => {
    if (selectedProvider && selectedModel) {
      onChange({
        provider: selectedProvider.providerName,
        model: selectedModel,
        apiKey: selectedProvider.isApiFieldRequired ? apiKey : undefined,
      });
    }
  }, [selectedProvider, selectedModel, apiKey]);

  return (
    <div className="flex items-center justify-between p-4 bg-gray-900/80 backdrop-blur-md border-b border-gray-700/50 shadow-sm">
      {/* Left side: Provider + Model */}
      <div className="flex items-center gap-3">
        <div className="flex items-center gap-2">
          <span className="text-md font-medium text-gray-300 whitespace-nowrap">Modulus AI</span>
        </div>
        
        <select
          value={selectedProvider?.providerName || ""}
          onChange={(e) => {
            const provider = providers.find((p) => p.providerName === e.target.value) || null;
            setSelectedProvider(provider);
            if (provider) setSelectedModel(provider.modelList[0] || "");
          }}
          className="input px-3 py-2 text-sm border-2 border-gray-600 focus:border-blue-400 focus:ring-2 focus:ring-blue-900/20 transition-all duration-200"
        >
          {providers.map((p) => (
            <option key={p.providerName} value={p.providerName}>
              {p.providerName}
            </option>
          ))}
        </select>

        <select
          value={selectedModel}
          onChange={(e) => setSelectedModel(e.target.value)}
          className="input px-3 py-2 text-sm border-2 border-gray-600 focus:border-blue-400 focus:ring-2 focus:ring-blue-900/20 transition-all duration-200 disabled:opacity-50"
          disabled={!selectedProvider}
        >
          {selectedProvider?.modelList.map((m) => (
            <option key={m} value={m}>
              {m}
            </option>
          ))}
        </select>

        {selectedProvider?.isApiFieldRequired && (
          <input
            type="password"
            placeholder="Enter API Key"
            value={apiKey}
            onChange={(e) => setApiKey(e.target.value)}
            className="input px-3 py-2 text-sm border-2 border-gray-600 focus:border-blue-400 focus:ring-2 focus:ring-blue-900/20 transition-all duration-200 min-w-[200px]"
          />
        )}
      </div>

      {/* Right side: Profile dropdown */}
      <div className="relative">
        <button
          onClick={() => setDropdownOpen((prev) => !prev)}
          className="flex items-center gap-2 px-4 py-2 bg-gradient-to-r from-gray-800 to-gray-700 rounded-xl hover:from-gray-700 hover:to-gray-600 transition-all duration-200 shadow-sm hover:shadow-md"
        >
          <div className="w-8 h-8 bg-gradient-to-br from-blue-500 to-indigo-600 rounded-lg flex items-center justify-center">
            <User className="w-4 h-4 text-white" />
          </div>
          <span className="text-sm font-medium text-gray-200">Profile</span>
          <ChevronDown size={16} className="text-gray-400" />
        </button>

        {dropdownOpen && (
          <div className="absolute right-0 mt-2 w-48 bg-gray-800/95 backdrop-blur-md border border-gray-700/50 rounded-xl shadow-xl animate-fade-in">
            <div className="p-2">
              <button
                className="flex items-center gap-3 w-full px-3 py-2 text-sm text-gray-200 hover:bg-gray-700 rounded-lg transition-colors duration-200"
                onClick={() => alert("Settings clicked")}
              >
                <Settings size={16} className="text-gray-400" />
                Settings
              </button>
              <button
                className="flex items-center gap-3 w-full px-3 py-2 text-sm text-red-400 hover:bg-red-900/20 rounded-lg transition-colors duration-200"
                onClick={onLogout}
              >
                <LogOut size={16} className="text-red-400" />
                Logout
              </button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default TopBar;
