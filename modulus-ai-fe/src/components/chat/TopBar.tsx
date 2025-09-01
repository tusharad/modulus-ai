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
        const data = await apiService.getModelProviders();
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
    <div className="flex items-center justify-between p-4 bg-white border-b shadow-sm">
      {/* Left side: Provider + Model */}
      <div className="flex items-center gap-4">
        <select
          value={selectedProvider?.providerName || ""}
          onChange={(e) => {
            const provider = providers.find((p) => p.providerName === e.target.value) || null;
            setSelectedProvider(provider);
            if (provider) setSelectedModel(provider.modelList[0] || "");
          }}
          className="border rounded-lg px-3 py-2"
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
          className="border rounded-lg px-3 py-2"
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
            className="border rounded-lg px-3 py-2 flex-1"
          />
        )}
      </div>

      {/* Right side: Profile dropdown */}
      <div className="relative">
        <button
          onClick={() => setDropdownOpen((prev) => !prev)}
          className="flex items-center gap-2 px-3 py-2 bg-gray-100 rounded-full hover:bg-gray-200"
        >
          <User className="w-5 h-5 text-gray-700" />
          <ChevronDown size={16} className="text-gray-600" />
        </button>

        {dropdownOpen && (
          <div className="absolute right-0 mt-2 w-40 bg-white border rounded-lg shadow-lg">
            <button
              className="flex items-center gap-2 w-full px-4 py-2 hover:bg-gray-100 text-left"
              onClick={() => alert("Settings clicked")}
            >
              <Settings size={16} /> Settings
            </button>
            <button
              className="flex items-center gap-2 w-full px-4 py-2 hover:bg-gray-100 text-left text-red-600"
              onClick={onLogout}
            >
              <LogOut size={16} /> Logout
            </button>
          </div>
        )}
      </div>
    </div>
  );
};

export default TopBar;
