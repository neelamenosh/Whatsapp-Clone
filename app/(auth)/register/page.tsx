"use client";

import { useState } from "react";
import Link from "next/link";
import { useRouter } from "next/navigation";
import { 
  Eye, 
  EyeOff, 
  Mail, 
  Lock, 
  MessageCircle,
  ArrowRight,
  User,
  Phone,
  Loader2,
  Check,
  X
} from "lucide-react";
import { registerUser, findUserByEmail } from "@/lib/user-store";

export default function RegisterPage() {
  const router = useRouter();
  const [step, setStep] = useState(1);
  const [showPassword, setShowPassword] = useState(false);
  const [showConfirmPassword, setShowConfirmPassword] = useState(false);
  const [isLoading, setIsLoading] = useState(false);
  const [agreedToTerms, setAgreedToTerms] = useState(false);
  
  const [formData, setFormData] = useState({
    firstName: "",
    lastName: "",
    email: "",
    phone: "",
    password: "",
    confirmPassword: "",
  });

  const [errors, setErrors] = useState<Record<string, string>>({});

  const passwordRequirements = [
    { label: "At least 8 characters", test: (p: string) => p.length >= 8 },
    { label: "One uppercase letter", test: (p: string) => /[A-Z]/.test(p) },
    { label: "One lowercase letter", test: (p: string) => /[a-z]/.test(p) },
    { label: "One number", test: (p: string) => /\d/.test(p) },
    { label: "One special character", test: (p: string) => /[!@#$%^&*(),.?":{}|<>]/.test(p) },
  ];

  const validateStep = (stepNumber: number) => {
    const newErrors: Record<string, string> = {};

    if (stepNumber === 1) {
      if (!formData.firstName.trim()) newErrors.firstName = "First name is required";
      if (!formData.lastName.trim()) newErrors.lastName = "Last name is required";
      if (!formData.email) {
        newErrors.email = "Email is required";
      } else if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(formData.email)) {
        newErrors.email = "Please enter a valid email";
      }
    }

    if (stepNumber === 2) {
      if (!formData.password) {
        newErrors.password = "Password is required";
      } else {
        const failedRequirements = passwordRequirements.filter(r => !r.test(formData.password));
        if (failedRequirements.length > 0) {
          newErrors.password = "Password does not meet requirements";
        }
      }
      if (formData.password !== formData.confirmPassword) {
        newErrors.confirmPassword = "Passwords do not match";
      }
      if (!agreedToTerms) {
        newErrors.terms = "You must agree to the terms and conditions";
      }
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleNext = () => {
    if (validateStep(step)) {
      setStep(step + 1);
    }
  };

  const handleBack = () => {
    setStep(step - 1);
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateStep(2)) return;
    
    setIsLoading(true);
    
    try {
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      const existingUser = findUserByEmail(formData.email);
      if (existingUser) {
        setErrors({ general: "An account with this email already exists. Please sign in." });
        setIsLoading(false);
        return;
      }
      
      registerUser({
        email: formData.email,
        firstName: formData.firstName,
        lastName: formData.lastName,
        phone: formData.phone,
      });
      
      router.push("/login?registered=true");
    } catch (error) {
      setErrors({ general: "Registration failed. Please try again." });
    } finally {
      setIsLoading(false);
    }
  };

  const getStepClass = (s: number) => {
    if (s === step) return "bg-primary text-primary-foreground";
    if (s < step) return "bg-primary/20 text-primary";
    return "bg-muted text-muted-foreground";
  };

  const getConnectorClass = (s: number) => {
    return s < step ? "bg-primary" : "bg-muted";
  };

  const getInputClass = (hasError: boolean) => {
    const base = "w-full pl-12 pr-4 py-3 bg-input border rounded-xl focus:outline-none focus:ring-2 focus:ring-primary/50";
    return hasError ? base + " border-destructive" : base + " border-border";
  };

  const getPasswordInputClass = (hasError: boolean) => {
    const base = "w-full pl-12 pr-12 py-3 bg-input border rounded-xl focus:outline-none focus:ring-2 focus:ring-primary/50";
    return hasError ? base + " border-destructive" : base + " border-border";
  };

  const renderStepIndicator = () => (
    <div className="flex items-center justify-center gap-2 mb-8">
      {[1, 2].map((s) => (
        <div key={s} className="flex items-center">
          <div className={"w-10 h-10 rounded-full flex items-center justify-center font-semibold transition-all " + getStepClass(s)}>
            {s < step ? <Check className="w-5 h-5" /> : s}
          </div>
          {s < 2 && (
            <div className={"w-12 h-1 mx-2 rounded " + getConnectorClass(s)} />
          )}
        </div>
      ))}
    </div>
  );

  const renderStep1 = () => (
    <div className="space-y-5">
      <div className="text-center lg:text-left">
        <h2 className="text-2xl font-bold text-foreground">Personal Information</h2>
        <p className="mt-1 text-muted-foreground">Tell us about yourself</p>
      </div>

      <div className="grid grid-cols-2 gap-4">
        <div className="space-y-2">
          <label className="block text-sm font-medium text-foreground">First Name</label>
          <div className="relative">
            <User className="absolute left-4 top-1/2 -translate-y-1/2 w-5 h-5 text-muted-foreground" />
            <input
              type="text"
              value={formData.firstName}
              onChange={(e) => setFormData({ ...formData, firstName: e.target.value })}
              className={getInputClass(!!errors.firstName)}
              placeholder="John"
            />
          </div>
          {errors.firstName && <p className="text-sm text-destructive">{errors.firstName}</p>}
        </div>

        <div className="space-y-2">
          <label className="block text-sm font-medium text-foreground">Last Name</label>
          <input
            type="text"
            value={formData.lastName}
            onChange={(e) => setFormData({ ...formData, lastName: e.target.value })}
            className={getInputClass(!!errors.lastName).replace("pl-12", "px-4")}
            placeholder="Doe"
          />
          {errors.lastName && <p className="text-sm text-destructive">{errors.lastName}</p>}
        </div>
      </div>

      <div className="space-y-2">
        <label className="block text-sm font-medium text-foreground">Email</label>
        <div className="relative">
          <Mail className="absolute left-4 top-1/2 -translate-y-1/2 w-5 h-5 text-muted-foreground" />
          <input
            type="email"
            value={formData.email}
            onChange={(e) => setFormData({ ...formData, email: e.target.value })}
            className={getInputClass(!!errors.email)}
            placeholder="john.doe@email.com"
          />
        </div>
        {errors.email && <p className="text-sm text-destructive">{errors.email}</p>}
      </div>

      <div className="space-y-2">
        <label className="block text-sm font-medium text-foreground">Phone Number (Optional)</label>
        <div className="relative">
          <Phone className="absolute left-4 top-1/2 -translate-y-1/2 w-5 h-5 text-muted-foreground" />
          <input
            type="tel"
            value={formData.phone}
            onChange={(e) => setFormData({ ...formData, phone: e.target.value })}
            className="w-full pl-12 pr-4 py-3 bg-input border border-border rounded-xl focus:outline-none focus:ring-2 focus:ring-primary/50"
            placeholder="+1 (555) 000-0000"
          />
        </div>
      </div>

      <button
        type="button"
        onClick={handleNext}
        className="w-full flex items-center justify-center gap-2 px-4 py-3 bg-primary text-primary-foreground font-semibold rounded-xl hover:bg-primary/90 transition-all"
      >
        Continue
        <ArrowRight className="w-5 h-5" />
      </button>
    </div>
  );

  const renderStep2 = () => (
    <div className="space-y-5">
      <div className="text-center lg:text-left">
        <h2 className="text-2xl font-bold text-foreground">Create Your Account</h2>
        <p className="mt-1 text-muted-foreground">Set up your secure password</p>
      </div>

      {errors.general && (
        <div className="p-4 bg-destructive/10 border border-destructive/20 rounded-xl text-destructive text-sm">
          {errors.general}
        </div>
      )}

      <div className="space-y-2">
        <label className="block text-sm font-medium text-foreground">Password</label>
        <div className="relative">
          <Lock className="absolute left-4 top-1/2 -translate-y-1/2 w-5 h-5 text-muted-foreground" />
          <input
            type={showPassword ? "text" : "password"}
            value={formData.password}
            onChange={(e) => setFormData({ ...formData, password: e.target.value })}
            className={getPasswordInputClass(!!errors.password)}
            placeholder="Create a strong password"
          />
          <button
            type="button"
            onClick={() => setShowPassword(!showPassword)}
            className="absolute right-4 top-1/2 -translate-y-1/2 text-muted-foreground hover:text-foreground"
          >
            {showPassword ? <EyeOff className="w-5 h-5" /> : <Eye className="w-5 h-5" />}
          </button>
        </div>
        
        <div className="grid grid-cols-1 gap-1.5 mt-3">
          {passwordRequirements.map((req, idx) => (
            <div key={idx} className="flex items-center gap-2 text-sm">
              {req.test(formData.password) ? (
                <Check className="w-4 h-4 text-green-500" />
              ) : (
                <X className="w-4 h-4 text-muted-foreground" />
              )}
              <span className={req.test(formData.password) ? "text-green-500" : "text-muted-foreground"}>
                {req.label}
              </span>
            </div>
          ))}
        </div>
      </div>

      <div className="space-y-2">
        <label className="block text-sm font-medium text-foreground">Confirm Password</label>
        <div className="relative">
          <Lock className="absolute left-4 top-1/2 -translate-y-1/2 w-5 h-5 text-muted-foreground" />
          <input
            type={showConfirmPassword ? "text" : "password"}
            value={formData.confirmPassword}
            onChange={(e) => setFormData({ ...formData, confirmPassword: e.target.value })}
            className={getPasswordInputClass(!!errors.confirmPassword)}
            placeholder="Confirm your password"
          />
          <button
            type="button"
            onClick={() => setShowConfirmPassword(!showConfirmPassword)}
            className="absolute right-4 top-1/2 -translate-y-1/2 text-muted-foreground hover:text-foreground"
          >
            {showConfirmPassword ? <EyeOff className="w-5 h-5" /> : <Eye className="w-5 h-5" />}
          </button>
        </div>
        {errors.confirmPassword && <p className="text-sm text-destructive">{errors.confirmPassword}</p>}
      </div>

      <div className="space-y-2">
        <label className="flex items-start gap-3 cursor-pointer">
          <input
            type="checkbox"
            checked={agreedToTerms}
            onChange={(e) => setAgreedToTerms(e.target.checked)}
            className="w-5 h-5 mt-0.5 rounded border-border text-primary focus:ring-primary/50"
          />
          <span className="text-sm text-muted-foreground">
            I agree to the{" "}
            <Link href="/terms" className="text-primary hover:underline">
              Terms of Service
            </Link>
            {" "}and{" "}
            <Link href="/privacy" className="text-primary hover:underline">
              Privacy Policy
            </Link>
          </span>
        </label>
        {errors.terms && <p className="text-sm text-destructive">{errors.terms}</p>}
      </div>

      <div className="flex gap-4">
        <button
          type="button"
          onClick={handleBack}
          className="flex-1 px-4 py-3 border border-border text-foreground font-semibold rounded-xl hover:bg-muted/50 transition-all"
          disabled={isLoading}
        >
          Back
        </button>
        <button
          type="submit"
          disabled={isLoading}
          className="flex-1 flex items-center justify-center gap-2 px-4 py-3 bg-primary text-primary-foreground font-semibold rounded-xl hover:bg-primary/90 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {isLoading ? (
            <>
              <Loader2 className="w-5 h-5 animate-spin" />
              Creating account...
            </>
          ) : (
            <>
              Create Account
              <ArrowRight className="w-5 h-5" />
            </>
          )}
        </button>
      </div>
    </div>
  );

  return (
    <div className="space-y-6">
      <div className="lg:hidden flex items-center justify-center gap-3 mb-4">
        <div className="w-12 h-12 rounded-2xl bg-primary flex items-center justify-center">
          <MessageCircle className="w-7 h-7 text-primary-foreground" />
        </div>
        <div>
          <h1 className="text-2xl font-bold text-foreground">WhatsApp</h1>
          <p className="text-sm text-muted-foreground">Messenger</p>
        </div>
      </div>

      {renderStepIndicator()}

      <form onSubmit={handleSubmit}>
        {step === 1 && renderStep1()}
        {step === 2 && renderStep2()}
      </form>

      <div className="text-center pt-4">
        <p className="text-muted-foreground">
          Already have an account?{" "}
          <Link 
            href="/login" 
            className="text-primary font-semibold hover:text-primary/80 transition-colors"
          >
            Sign in
          </Link>
        </p>
      </div>
    </div>
  );
}
