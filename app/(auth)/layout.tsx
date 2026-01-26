"use client";

import { MessageCircle, Shield, Building2 } from "lucide-react";

export default function AuthLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div className="h-[100dvh] flex overflow-hidden">
      {/* Left Panel - Branding */}
      <div className="hidden lg:flex lg:w-1/2 bg-gradient-to-br from-primary/90 via-primary to-primary/80 relative overflow-hidden">
        {/* Background Pattern */}
        <div className="absolute inset-0 opacity-10">
          <div className="absolute top-20 left-20 w-64 h-64 rounded-full bg-white/20 blur-3xl" />
          <div className="absolute bottom-32 right-20 w-96 h-96 rounded-full bg-white/10 blur-3xl" />
          <div className="absolute top-1/2 left-1/3 w-48 h-48 rounded-full bg-white/15 blur-2xl" />
        </div>
        
        {/* Content */}
        <div className="relative z-10 flex flex-col justify-between p-12 text-white">
          {/* Logo */}
          <div className="flex items-center gap-3">
            <div className="w-12 h-12 rounded-2xl bg-white/20 backdrop-blur-sm flex items-center justify-center">
              <MessageCircle className="w-7 h-7" />
            </div>
            <div>
              <h1 className="text-2xl font-bold">WhatsApp</h1>
              <p className="text-sm text-white/80">Enterprise</p>
            </div>
          </div>
          
          {/* Main Content */}
          <div className="space-y-8">
            <div>
              <h2 className="text-4xl font-bold leading-tight">
                Secure Business
                <br />
                Communication
              </h2>
              <p className="mt-4 text-lg text-white/80 max-w-md">
                Connect your teams with enterprise-grade messaging, 
                end-to-end encryption, and compliance tools built for business.
              </p>
            </div>
            
            {/* Features */}
            <div className="space-y-4">
              <div className="flex items-center gap-4">
                <div className="w-10 h-10 rounded-xl bg-white/20 flex items-center justify-center">
                  <Shield className="w-5 h-5" />
                </div>
                <div>
                  <p className="font-semibold">End-to-End Encryption</p>
                  <p className="text-sm text-white/70">Your messages are always secure</p>
                </div>
              </div>
              <div className="flex items-center gap-4">
                <div className="w-10 h-10 rounded-xl bg-white/20 flex items-center justify-center">
                  <Building2 className="w-5 h-5" />
                </div>
                <div>
                  <p className="font-semibold">Enterprise Controls</p>
                  <p className="text-sm text-white/70">Admin tools & compliance features</p>
                </div>
              </div>
            </div>
          </div>
          
          {/* Footer */}
          <div className="text-sm text-white/60">
            © 2026 WhatsApp Enterprise. All rights reserved.
          </div>
        </div>
      </div>
      
      {/* Right Panel - Auth Forms */}
      <div className="flex-1 h-full overflow-y-auto bg-background">
        <div className="min-h-full flex items-center justify-center p-6 sm:p-8">
          <div className="w-full max-w-md py-6 sm:py-8">
            {children}
          </div>
        </div>
      </div>
    </div>
  );
}
