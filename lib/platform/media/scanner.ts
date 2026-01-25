// Media scanning service for content moderation and malware detection.
// Integrates with external scanning services for production use.

export interface ScanResult {
  scanId: string;
  status: 'clean' | 'flagged' | 'quarantined' | 'pending' | 'error';
  
  // For flagged/quarantined content
  flags?: ContentFlag[];
  
  // Malware detection
  malware?: {
    detected: boolean;
    threatName?: string;
    severity?: 'low' | 'medium' | 'high' | 'critical';
  };
  
  // Metadata extracted
  metadata?: {
    mimeType?: string;
    width?: number;
    height?: number;
    duration?: number;
    hasAudio?: boolean;
    textContent?: string; // OCR result for images
  };
  
  scannedAt: string;
  scanDurationMs: number;
}

export interface ContentFlag {
  type: 'adult' | 'violence' | 'hate' | 'self-harm' | 'copyright' | 'pii' | 'spam';
  confidence: number; // 0-1
  description?: string;
}

export interface MediaScannerConfig {
  // Enable/disable specific scan types
  enableMalwareScan: boolean;
  enableContentModeration: boolean;
  enableOcr: boolean;
  
  // Thresholds
  moderationThreshold: number; // 0-1, flag content above this confidence
  
  // Actions
  autoQuarantineMalware: boolean;
  autoQuarantineFlagged: boolean;
}

export interface MediaScanner {
  /**
   * Scan media before it's stored.
   * Returns scan result; content should not be stored if quarantined.
   */
  scanMedia(
    tenantId: string,
    media: MediaToScan
  ): Promise<ScanResult>;
  
  /**
   * Get scan result for previously scanned media.
   */
  getScanResult(scanId: string): Promise<ScanResult | null>;
  
  /**
   * Rescan media (e.g., after policy change).
   */
  rescan(scanId: string): Promise<ScanResult>;
  
  /**
   * Report false positive/negative.
   */
  reportFeedback(
    scanId: string,
    feedback: 'false_positive' | 'false_negative',
    notes?: string
  ): Promise<void>;
}

export interface MediaToScan {
  mediaId: string;
  filename: string;
  mimeType: string;
  size: number;
  content: Buffer | ReadableStream;
  uploadedBy: string;
  chatId?: string;
}

// Demo implementation - all media passes scan
export class DemoMediaScanner implements MediaScanner {
  private results: Map<string, ScanResult> = new Map();
  
  async scanMedia(tenantId: string, media: MediaToScan): Promise<ScanResult> {
    const scanId = `scan_${media.mediaId}`;
    const startTime = Date.now();
    
    // Simulate scan time
    await new Promise(r => setTimeout(r, 100));
    
    const result: ScanResult = {
      scanId,
      status: 'clean',
      metadata: {
        mimeType: media.mimeType,
      },
      scannedAt: new Date().toISOString(),
      scanDurationMs: Date.now() - startTime,
    };
    
    // Demo: flag suspicious filenames
    if (media.filename.includes('virus') || media.filename.includes('malware')) {
      result.status = 'quarantined';
      result.malware = {
        detected: true,
        threatName: 'Demo.Test.Threat',
        severity: 'high',
      };
    }
    
    this.results.set(scanId, result);
    return result;
  }
  
  async getScanResult(scanId: string): Promise<ScanResult | null> {
    return this.results.get(scanId) ?? null;
  }
  
  async rescan(scanId: string): Promise<ScanResult> {
    const existing = this.results.get(scanId);
    if (!existing) {
      throw new Error(`Scan result not found: ${scanId}`);
    }
    // In demo mode, just return existing result
    return existing;
  }
  
  async reportFeedback(
    scanId: string,
    feedback: 'false_positive' | 'false_negative',
    notes?: string
  ): Promise<void> {
    console.log(`[MEDIA_SCANNER] Feedback for ${scanId}: ${feedback}`, notes ?? '');
  }
}

// Singleton
let mediaScanner: MediaScanner | null = null;

export function getMediaScanner(): MediaScanner {
  if (!mediaScanner) {
    mediaScanner = new DemoMediaScanner();
  }
  return mediaScanner;
}
