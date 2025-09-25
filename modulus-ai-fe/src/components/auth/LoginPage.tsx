import React, { useEffect, useState } from 'react';
import { MessageSquare, Eye, EyeOff } from 'lucide-react';
import { apiService } from '../../services/api.service';
import { setCookie } from '../../services/cookies';
import type { AuthTokens } from '../../types';
import { useNavigate, Link } from 'react-router';
import { getCookie } from '../../services/cookies.ts';

const LoginPage: React.FC = () => {
  const navigate = useNavigate();
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [showPassword, setShowPassword] = useState(false);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState('');
  const [msg, setMsg] = useState<string|null>(null);

  useEffect(() => {
    const token = getCookie("accessToken")
    if (token) {
        console.log("user already logged in, redirecting to chat");
        navigate("/chat")
    }
  },[]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setError('')
    setMsg('')

    try {
      const tokens: AuthTokens = await apiService.login({
        loginEmail: email,
        loginPassword: password,
      });
      setCookie('accessToken', tokens.accessToken);
      setCookie('refreshToken', tokens.refreshToken);
      setMsg("Loging successful! redirecting to chat page...");
      console.log("redirecting to chat");
      navigate('/chat');
    } catch (err) {
      setError(`Invalid email or password ${err}`);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="min-h-screen flex items-center justify-center p-4 relative overflow-hidden">
      <div className="relative z-10 w-full max-w-md">
        <div className="card p-8 animate-fade-in">
          {/* Header */}
          <div className="text-center mb-8">
            <div className="relative mb-6">
              <div className="w-20 h-20 mx-auto bg-gradient-to-br from-blue-500 to-indigo-600 rounded-2xl flex items-center justify-center shadow-lg">
                <MessageSquare className="text-white" size={32} />
              </div>
            </div>
            <h1 className="text-3xl font-bold bg-gradient-to-r from-gray-100 to-gray-300 bg-clip-text text-transparent mb-2">
              Welcome Back to Modulus AI
            </h1>
            <p className="text-gray-400 text-sm">
              Sign in to continue your AI conversations
            </p>
          </div>

          {/* Form */}
          <form onSubmit={handleSubmit} className="space-y-6">
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-semibold text-gray-300 mb-2">
                  Email Address
                </label>
                <div className="relative">
                  <input
                    type="email"
                    value={email}
                    onChange={(e) => setEmail(e.target.value)}
                    className="input pl-4 pr-4 py-3 text-sm border-2 focus:border-blue-400 focus:ring-4 focus:ring-blue-900 transition-all duration-200"
                    placeholder="Enter your email address"
                    required
                  />
                </div>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-300 mb-2">
                  Password
                </label>
                <div className="relative">
                  <input
                    type={showPassword ? 'text' : 'password'}
                    value={password}
                    onChange={(e) => setPassword(e.target.value)}
                    className="input pl-4 pr-12 py-3 text-sm border-2 focus:border-blue-400 focus:ring-4 focus:ring-blue-900 transition-all duration-200"
                    placeholder="Enter your password"
                    required
                  />
                  <button
                    type="button"
                    onClick={() => setShowPassword(!showPassword)}
                    className="absolute right-3 top-1/2 transform -translate-y-1/2 text-gray-500 hover:text-gray-300 transition-colors duration-200"
                  >
                    {showPassword ? <EyeOff size={20} /> : <Eye size={20} />}
                  </button>
                </div>
              </div>
            </div>

            {error && (
              <div className="bg-red-900/20 border-l-4 border-red-500 p-4 rounded-lg animate-fade-in">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <div className="w-5 h-5 bg-red-500 rounded-full flex items-center justify-center">
                      <span className="text-white text-xs font-bold">!</span>
                    </div>
                  </div>
                  <div className="ml-3">
                    <p className="text-sm text-red-300">{error}</p>
                  </div>
                </div>
              </div>
            )}

            {msg && (
              <div className="bg-green-900/20 border-l-4 border-green-500 p-4 rounded-lg animate-fade-in">
                <div className="flex">
                  <div className="ml-3">
                    <p className="text-sm text-green-300">{msg}</p>
                    <p className="text-sm text-green-300">If not redirected, please click 
                        <Link to="/chat" className="text-emerald-400 hover:text-emerald-300"> here</Link>
                    </p>
                  </div>
                </div>
              </div>
            )}

            <button
              type="submit"
              disabled={loading}
              className="w-full btn btn-primary py-3 px-6 text-sm font-semibold rounded-xl shadow-lg hover:shadow-xl transform hover:-translate-y-0.5 transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none"
            >
              {loading ? (
                <div className="flex items-center justify-center">
                  <div className="w-5 h-5 border-2 border-white border-t-transparent rounded-full animate-spin mr-2"></div>
                  Signing in...
                </div>
              ) : (
                'Sign In'
              )}
            </button>
          </form>

          {/* Footer */}
          <div className="mt-8 text-center">
            <p className="text-sm text-gray-400">
              Don't have an account?{' '}
              <Link 
                to="/register" 
                className="font-semibold text-blue-400 hover:text-blue-300 transition-colors duration-200"
              >
                Create one here
              </Link>
            </p>
          </div>
        </div>
      </div>
    </div>
  );
};

export default LoginPage;
