'use client';

import * as React from 'react';
import type { AppSettings } from '@/lib/settings';
import { defaultSettings } from '@/lib/settings';
import { readJson, writeJson } from '@/lib/persist';

type SettingsContextValue = {
  settings: AppSettings;
  updateSettings: (updater: (prev: AppSettings) => AppSettings) => void;
  resetSettings: () => void;
};

const SettingsContext = React.createContext<SettingsContextValue | null>(null);

const STORAGE_KEY = 'settings';

export function SettingsProvider({ children }: { children: React.ReactNode }) {
  const [settings, setSettings] = React.useState<AppSettings>(() => {
    const stored = readJson<AppSettings>(STORAGE_KEY);
    if (!stored) return defaultSettings;

    const merged: AppSettings = {
      ...defaultSettings,
      ...stored,
      privacy: { ...defaultSettings.privacy, ...stored.privacy },
      security: { ...defaultSettings.security, ...stored.security },
      accessibility: { ...defaultSettings.accessibility, ...stored.accessibility },
      integrations: { ...defaultSettings.integrations, ...stored.integrations },
    };

    // Persist the merged shape so newly-added defaults apply across reloads.
    writeJson(STORAGE_KEY, merged);
    return merged;
  });

  const updateSettings = React.useCallback(
    (updater: (prev: AppSettings) => AppSettings) => {
      setSettings((prev) => {
        const next = updater(prev);
        writeJson(STORAGE_KEY, next);
        return next;
      });
    },
    []
  );

  const resetSettings = React.useCallback(() => {
    setSettings(defaultSettings);
    writeJson(STORAGE_KEY, defaultSettings);
  }, []);

  // Apply accessibility classes to <html> so CSS can react.
  React.useEffect(() => {
    const root = document.documentElement;
    root.classList.toggle('high-contrast', settings.accessibility.highContrast);
    root.classList.toggle(
      'reduced-transparency',
      settings.accessibility.reducedTransparency
    );
    root.classList.toggle('reduced-motion', settings.accessibility.reducedMotion);
  }, [
    settings.accessibility.highContrast,
    settings.accessibility.reducedTransparency,
    settings.accessibility.reducedMotion,
  ]);

  const value = React.useMemo(
    () => ({ settings, updateSettings, resetSettings }),
    [settings, updateSettings, resetSettings]
  );

  return <SettingsContext.Provider value={value}>{children}</SettingsContext.Provider>;
}

export function useSettings() {
  const ctx = React.useContext(SettingsContext);
  if (!ctx) throw new Error('useSettings must be used within SettingsProvider');
  return ctx;
}
