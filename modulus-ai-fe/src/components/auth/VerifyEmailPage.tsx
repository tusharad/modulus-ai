import React, { useState } from "react";
import { useNavigate, useLocation } from "react-router";
import { apiService } from "../../services/api.service";
import { Mail, Shield, CheckCircle } from "lucide-react";

const VerifyEmailPage: React.FC = () => {
  const navigate = useNavigate();
  const location = useLocation();
  const queryParams = new URLSearchParams(location.search);
  const email = queryParams.get("email") || "";

  const [otp, setOtp] = useState("");
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState("");

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!/^\d{6}$/.test(otp)) {
      setError("OTP must be a 6-digit number");
      return;
    }

    setLoading(true);
    setError("");

    try {
      await apiService.verifyOTP({ verifyEmail: email, verifyOTP: Number(otp) });
      navigate("/login");
    } catch (err) {
      setError(`Verification failed. Please try again. ${err}`);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="min-h-screen flex items-center justify-center p-4 relative overflow-hidden">
      {/* Background Elements */}
      <div className="absolute inset-0 bg-gradient-to-br from-amber-50 via-white to-orange-50"></div>
      <div className="absolute top-0 left-0 w-full h-full overflow-hidden">
        <div className="absolute -top-40 -right-40 w-80 h-80 bg-gradient-to-br from-amber-400/20 to-orange-600/20 rounded-full blur-3xl"></div>
        <div className="absolute -bottom-40 -left-40 w-80 h-80 bg-gradient-to-tr from-yellow-400/20 to-red-600/20 rounded-full blur-3xl"></div>
        <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 w-96 h-96 bg-gradient-to-r from-amber-400/10 to-orange-600/10 rounded-full blur-3xl"></div>
      </div>

      <div className="relative z-10 w-full max-w-md">
        <div className="card p-8 animate-fade-in">
          {/* Header */}
          <div className="text-center mb-8">
            <div className="relative mb-6">
              <div className="w-20 h-20 mx-auto bg-gradient-to-br from-amber-500 to-orange-600 rounded-2xl flex items-center justify-center shadow-lg">
                <Mail className="text-white" size={32} />
              </div>
              <div className="absolute -top-1 -right-1 w-6 h-6 bg-gradient-to-r from-green-400 to-emerald-500 rounded-full flex items-center justify-center">
                <Shield className="w-3 h-3 text-white" />
              </div>
            </div>
            <h1 className="text-3xl font-bold bg-gradient-to-r from-gray-900 to-gray-700 bg-clip-text text-transparent mb-2">
              Verify Your Email
            </h1>
            <p className="text-gray-600 text-sm">
              We've sent a 6-digit code to{' '}
              <span className="font-semibold text-amber-600">{email}</span>
            </p>
          </div>

          {/* Form */}
          <form onSubmit={handleSubmit} className="space-y-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-2">
                Verification Code
              </label>
              <div className="relative">
                <input
                  type="text"
                  maxLength={6}
                  value={otp}
                  onChange={(e) => setOtp(e.target.value)}
                  className="input pl-4 pr-4 py-4 text-center text-2xl font-mono tracking-widest border-2 border-gray-200 focus:border-amber-500 focus:ring-4 focus:ring-amber-100 transition-all duration-200"
                  placeholder="000000"
                  required
                />
                <div className="absolute inset-y-0 right-0 flex items-center pr-3">
                  <CheckCircle className="w-5 h-5 text-gray-400" />
                </div>
              </div>
              <p className="text-xs text-gray-500 mt-2 text-center">
                Enter the 6-digit code from your email
              </p>
            </div>

            {error && (
              <div className="bg-red-50 border-l-4 border-red-400 p-4 rounded-lg animate-fade-in">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <div className="w-5 h-5 bg-red-400 rounded-full flex items-center justify-center">
                      <span className="text-white text-xs font-bold">!</span>
                    </div>
                  </div>
                  <div className="ml-3">
                    <p className="text-sm text-red-700">{error}</p>
                  </div>
                </div>
              </div>
            )}

            <button
              type="submit"
              disabled={loading}
              className="w-full bg-gradient-to-r from-amber-600 to-orange-600 text-white py-3 px-6 text-sm font-semibold rounded-xl shadow-lg hover:shadow-xl transform hover:-translate-y-0.5 transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none"
            >
              {loading ? (
                <div className="flex items-center justify-center">
                  <div className="w-5 h-5 border-2 border-white border-t-transparent rounded-full animate-spin mr-2"></div>
                  Verifying...
                </div>
              ) : (
                'Verify Email'
              )}
            </button>
          </form>

          {/* Footer */}
          <div className="mt-8 text-center">
            <p className="text-xs text-gray-500">
              Didn't receive the code? Check your spam folder or{' '}
              <button className="text-amber-600 hover:text-amber-700 font-semibold">
                resend
              </button>
            </p>
          </div>
        </div>
      </div>
    </div>
  );
};

export default VerifyEmailPage;
