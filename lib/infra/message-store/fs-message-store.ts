import type { MessageStore } from '@/lib/domain/messaging/store';
import type { MessageDTO } from '@/lib/contracts/messaging';
import { appendMessage as appendFs, listMessages as listFs } from '@/lib/server/message-store';

// Dev/demo implementation backed by a local JSON file on disk.
export const fsMessageStore: MessageStore = {
  async listMessages(chatId: string, sinceMs?: number): Promise<MessageDTO[]> {
    return listFs(chatId, sinceMs);
  },

  async appendMessage(message: MessageDTO): Promise<MessageDTO> {
    return appendFs(message);
  },
};
