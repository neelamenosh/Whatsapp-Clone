import type { MessageDTO } from '@/lib/contracts/messaging';

export interface MessageStore {
  listMessages(chatId: string, sinceMs?: number): Promise<MessageDTO[]>;
  appendMessage(message: MessageDTO): Promise<MessageDTO>;
}
