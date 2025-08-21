import React, { useState } from "react";
import { useNavigate, useLocation } from "react-router";
import { apiService } from "../../services/api.service";

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
    <div className="min-h-screen bg-gradient-to-br from-yellow-50 to-orange-100 flex items-center justify-center p-4">
      <div className="bg-white rounded-2xl shadow-xl w-full max-w-md p-8">
        <div className="text-center mb-8">
          <h1 className="text-2xl font-bold text-gray-900">Verify Your Email</h1>
          <p className="text-gray-600 mt-2">
            Enter the 6-digit code sent to <span className="font-medium">{email}</span>
          </p>
        </div>

        <form onSubmit={handleSubmit} className="space-y-6">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">OTP Code</label>
            <input
              type="text"
              maxLength={6}
              value={otp}
              onChange={(e) => setOtp(e.target.value)}
              className="w-full px-4 py-3 border border-gray-300 rounded-lg focus:ring-2 
              focus:ring-orange-500 focus:border-transparent transition-all text-center tracking-widest text-lg"
              placeholder="123456"
              required
            />
          </div>

          {error && (
            <div className="text-red-600 text-sm bg-red-50 p-3 rounded-lg">{error}</div>
          )}

          <button
            type="submit"
            disabled={loading}
            className="w-full bg-gradient-to-r from-orange-600 to-yellow-600 text-white py-3 px-4 rounded-lg 
            hover:from-orange-700 hover:to-yellow-700 transition-all font-medium disabled:opacity-50"
          >
            {loading ? "Verifying..." : "Verify Email"}
          </button>
        </form>
      </div>
    </div>
  );
};

export default VerifyEmailPage;
