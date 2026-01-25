// Collaboration Service
// Handles collaborative documents and whiteboards

import type {
  CollaborativeDocument,
  DocumentChange,
  CollaborativeWhiteboard,
  WhiteboardElement,
  WhiteboardAction,
} from '@/lib/contracts/collaboration';
import { getEventBus, type EventBus } from '@/lib/domain/events/bus';

// ============================================================================
// EVENTS
// ============================================================================

export const COLLAB_EVENTS = {
  // Documents
  DOCUMENT_CREATED: 'collab:document_created',
  DOCUMENT_UPDATED: 'collab:document_updated',
  DOCUMENT_CHANGE: 'collab:document_change',
  DOCUMENT_COLLABORATOR_JOINED: 'collab:document_collaborator_joined',
  DOCUMENT_COLLABORATOR_LEFT: 'collab:document_collaborator_left',
  
  // Whiteboards
  WHITEBOARD_CREATED: 'collab:whiteboard_created',
  WHITEBOARD_ELEMENT_ADDED: 'collab:whiteboard_element_added',
  WHITEBOARD_ELEMENT_UPDATED: 'collab:whiteboard_element_updated',
  WHITEBOARD_ELEMENT_REMOVED: 'collab:whiteboard_element_removed',
  WHITEBOARD_ACTION: 'collab:whiteboard_action',
} as const;

// ============================================================================
// DOCUMENT STORE INTERFACE
// ============================================================================

export interface DocumentStore {
  create(doc: Omit<CollaborativeDocument, 'id'>): Promise<CollaborativeDocument>;
  get(docId: string): Promise<CollaborativeDocument | null>;
  getByChat(chatId: string): Promise<CollaborativeDocument[]>;
  update(docId: string, updates: Partial<CollaborativeDocument>): Promise<CollaborativeDocument | null>;
  delete(docId: string): Promise<boolean>;
  
  // Changes
  addChange(docId: string, change: Omit<DocumentChange, 'id'>): Promise<DocumentChange>;
  getChanges(docId: string, since?: string): Promise<DocumentChange[]>;
  
  // Collaborators
  addCollaborator(docId: string, userId: string): Promise<void>;
  removeCollaborator(docId: string, userId: string): Promise<void>;
  getActiveCollaborators(docId: string): Promise<string[]>;
}

// ============================================================================
// WHITEBOARD STORE INTERFACE
// ============================================================================

export interface WhiteboardStore {
  create(whiteboard: Omit<CollaborativeWhiteboard, 'id'>): Promise<CollaborativeWhiteboard>;
  get(whiteboardId: string): Promise<CollaborativeWhiteboard | null>;
  getByChat(chatId: string): Promise<CollaborativeWhiteboard[]>;
  update(whiteboardId: string, updates: Partial<CollaborativeWhiteboard>): Promise<CollaborativeWhiteboard | null>;
  delete(whiteboardId: string): Promise<boolean>;
  
  // Elements
  addElement(whiteboardId: string, element: WhiteboardElement): Promise<void>;
  updateElement(whiteboardId: string, elementId: string, updates: Partial<WhiteboardElement>): Promise<void>;
  removeElement(whiteboardId: string, elementId: string): Promise<void>;
  getElements(whiteboardId: string): Promise<WhiteboardElement[]>;
  
  // Actions
  recordAction(whiteboardId: string, action: Omit<WhiteboardAction, 'id'>): Promise<WhiteboardAction>;
  getActions(whiteboardId: string, since?: string): Promise<WhiteboardAction[]>;
}

// ============================================================================
// IN-MEMORY DOCUMENT STORE
// ============================================================================

export class MemoryDocumentStore implements DocumentStore {
  private documents = new Map<string, CollaborativeDocument>();
  private changes = new Map<string, DocumentChange[]>(); // docId -> changes
  private collaborators = new Map<string, Set<string>>(); // docId -> userIds
  
  async create(data: Omit<CollaborativeDocument, 'id'>): Promise<CollaborativeDocument> {
    const id = `doc-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const doc: CollaborativeDocument = { id, ...data };
    this.documents.set(id, doc);
    this.changes.set(id, []);
    this.collaborators.set(id, new Set([data.createdBy]));
    return doc;
  }
  
  async get(docId: string): Promise<CollaborativeDocument | null> {
    return this.documents.get(docId) ?? null;
  }
  
  async getByChat(chatId: string): Promise<CollaborativeDocument[]> {
    return Array.from(this.documents.values())
      .filter(d => d.chatId === chatId)
      .sort((a, b) => new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime());
  }
  
  async update(docId: string, updates: Partial<CollaborativeDocument>): Promise<CollaborativeDocument | null> {
    const existing = this.documents.get(docId);
    if (!existing) return null;
    
    const updated: CollaborativeDocument = {
      ...existing,
      ...updates,
      id: docId,
      updatedAt: new Date().toISOString(),
    };
    this.documents.set(docId, updated);
    return updated;
  }
  
  async delete(docId: string): Promise<boolean> {
    this.changes.delete(docId);
    this.collaborators.delete(docId);
    return this.documents.delete(docId);
  }
  
  async addChange(docId: string, data: Omit<DocumentChange, 'id'>): Promise<DocumentChange> {
    const id = `change-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const change: DocumentChange = { id, ...data };
    
    if (!this.changes.has(docId)) {
      this.changes.set(docId, []);
    }
    this.changes.get(docId)!.push(change);
    
    return change;
  }
  
  async getChanges(docId: string, since?: string): Promise<DocumentChange[]> {
    const changes = this.changes.get(docId) ?? [];
    if (!since) return changes;
    
    const sinceTime = new Date(since).getTime();
    return changes.filter(c => new Date(c.timestamp).getTime() > sinceTime);
  }
  
  async addCollaborator(docId: string, userId: string): Promise<void> {
    if (!this.collaborators.has(docId)) {
      this.collaborators.set(docId, new Set());
    }
    this.collaborators.get(docId)!.add(userId);
  }
  
  async removeCollaborator(docId: string, userId: string): Promise<void> {
    this.collaborators.get(docId)?.delete(userId);
  }
  
  async getActiveCollaborators(docId: string): Promise<string[]> {
    return Array.from(this.collaborators.get(docId) ?? []);
  }
}

// ============================================================================
// IN-MEMORY WHITEBOARD STORE
// ============================================================================

export class MemoryWhiteboardStore implements WhiteboardStore {
  private whiteboards = new Map<string, CollaborativeWhiteboard>();
  private elements = new Map<string, Map<string, WhiteboardElement>>(); // wbId -> elementId -> element
  private actions = new Map<string, WhiteboardAction[]>(); // wbId -> actions
  
  async create(data: Omit<CollaborativeWhiteboard, 'id'>): Promise<CollaborativeWhiteboard> {
    const id = `wb-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const whiteboard: CollaborativeWhiteboard = { id, ...data };
    this.whiteboards.set(id, whiteboard);
    this.elements.set(id, new Map());
    this.actions.set(id, []);
    return whiteboard;
  }
  
  async get(whiteboardId: string): Promise<CollaborativeWhiteboard | null> {
    return this.whiteboards.get(whiteboardId) ?? null;
  }
  
  async getByChat(chatId: string): Promise<CollaborativeWhiteboard[]> {
    return Array.from(this.whiteboards.values())
      .filter(w => w.chatId === chatId)
      .sort((a, b) => new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime());
  }
  
  async update(whiteboardId: string, updates: Partial<CollaborativeWhiteboard>): Promise<CollaborativeWhiteboard | null> {
    const existing = this.whiteboards.get(whiteboardId);
    if (!existing) return null;
    
    const updated: CollaborativeWhiteboard = {
      ...existing,
      ...updates,
      id: whiteboardId,
      updatedAt: new Date().toISOString(),
    };
    this.whiteboards.set(whiteboardId, updated);
    return updated;
  }
  
  async delete(whiteboardId: string): Promise<boolean> {
    this.elements.delete(whiteboardId);
    this.actions.delete(whiteboardId);
    return this.whiteboards.delete(whiteboardId);
  }
  
  async addElement(whiteboardId: string, element: WhiteboardElement): Promise<void> {
    if (!this.elements.has(whiteboardId)) {
      this.elements.set(whiteboardId, new Map());
    }
    this.elements.get(whiteboardId)!.set(element.id, element);
  }
  
  async updateElement(whiteboardId: string, elementId: string, updates: Partial<WhiteboardElement>): Promise<void> {
    const elementMap = this.elements.get(whiteboardId);
    if (!elementMap) return;
    
    const existing = elementMap.get(elementId);
    if (existing) {
      elementMap.set(elementId, { ...existing, ...updates, id: elementId });
    }
  }
  
  async removeElement(whiteboardId: string, elementId: string): Promise<void> {
    this.elements.get(whiteboardId)?.delete(elementId);
  }
  
  async getElements(whiteboardId: string): Promise<WhiteboardElement[]> {
    return Array.from(this.elements.get(whiteboardId)?.values() ?? []);
  }
  
  async recordAction(whiteboardId: string, data: Omit<WhiteboardAction, 'id'>): Promise<WhiteboardAction> {
    const id = `action-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const action: WhiteboardAction = { id, ...data };
    
    if (!this.actions.has(whiteboardId)) {
      this.actions.set(whiteboardId, []);
    }
    this.actions.get(whiteboardId)!.push(action);
    
    return action;
  }
  
  async getActions(whiteboardId: string, since?: string): Promise<WhiteboardAction[]> {
    const actions = this.actions.get(whiteboardId) ?? [];
    if (!since) return actions;
    
    const sinceTime = new Date(since).getTime();
    return actions.filter(a => new Date(a.timestamp).getTime() > sinceTime);
  }
}

// ============================================================================
// COLLABORATION SERVICE
// ============================================================================

export interface CollaborationService {
  // Documents
  createDocument(
    chatId: string,
    createdBy: string,
    title: string,
    type?: 'text' | 'markdown' | 'richtext'
  ): Promise<CollaborativeDocument>;
  getDocument(docId: string): Promise<CollaborativeDocument | null>;
  getChatDocuments(chatId: string): Promise<CollaborativeDocument[]>;
  updateDocumentContent(docId: string, userId: string, content: string): Promise<CollaborativeDocument | null>;
  applyDocumentChange(docId: string, change: Omit<DocumentChange, 'id'>): Promise<DocumentChange>;
  joinDocument(docId: string, userId: string): Promise<void>;
  leaveDocument(docId: string, userId: string): Promise<void>;
  getDocumentCollaborators(docId: string): Promise<string[]>;
  
  // Whiteboards
  createWhiteboard(
    chatId: string,
    createdBy: string,
    name: string
  ): Promise<CollaborativeWhiteboard>;
  getWhiteboard(whiteboardId: string): Promise<CollaborativeWhiteboard | null>;
  getChatWhiteboards(chatId: string): Promise<CollaborativeWhiteboard[]>;
  addWhiteboardElement(whiteboardId: string, userId: string, element: Omit<WhiteboardElement, 'id' | 'createdBy' | 'createdAt'>): Promise<WhiteboardElement>;
  updateWhiteboardElement(whiteboardId: string, elementId: string, userId: string, updates: Partial<WhiteboardElement>): Promise<void>;
  removeWhiteboardElement(whiteboardId: string, elementId: string, userId: string): Promise<void>;
  getWhiteboardElements(whiteboardId: string): Promise<WhiteboardElement[]>;
}

export class CollaborationServiceImpl implements CollaborationService {
  private docStore: DocumentStore;
  private wbStore: WhiteboardStore;
  private eventBus: EventBus;
  
  constructor(docStore: DocumentStore, wbStore: WhiteboardStore, eventBus: EventBus) {
    this.docStore = docStore;
    this.wbStore = wbStore;
    this.eventBus = eventBus;
  }
  
  // ==================== DOCUMENTS ====================
  
  async createDocument(
    chatId: string,
    createdBy: string,
    title: string,
    type: 'text' | 'markdown' | 'richtext' = 'text'
  ): Promise<CollaborativeDocument> {
    const doc = await this.docStore.create({
      chatId,
      title,
      content: '',
      type,
      version: 1,
      createdBy,
      collaborators: [createdBy],
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    });
    
    this.eventBus.publish({
      type: COLLAB_EVENTS.DOCUMENT_CREATED,
      payload: doc,
    });
    
    return doc;
  }
  
  async getDocument(docId: string): Promise<CollaborativeDocument | null> {
    return this.docStore.get(docId);
  }
  
  async getChatDocuments(chatId: string): Promise<CollaborativeDocument[]> {
    return this.docStore.getByChat(chatId);
  }
  
  async updateDocumentContent(
    docId: string,
    userId: string,
    content: string
  ): Promise<CollaborativeDocument | null> {
    const doc = await this.docStore.get(docId);
    if (!doc) return null;
    
    const updated = await this.docStore.update(docId, {
      content,
      version: doc.version + 1,
      lastEditedBy: userId,
    });
    
    if (updated) {
      this.eventBus.publish({
        type: COLLAB_EVENTS.DOCUMENT_UPDATED,
        payload: updated,
      });
    }
    
    return updated;
  }
  
  async applyDocumentChange(docId: string, data: Omit<DocumentChange, 'id'>): Promise<DocumentChange> {
    const change = await this.docStore.addChange(docId, data);
    
    this.eventBus.publish({
      type: COLLAB_EVENTS.DOCUMENT_CHANGE,
      payload: { docId, change },
    });
    
    return change;
  }
  
  async joinDocument(docId: string, userId: string): Promise<void> {
    await this.docStore.addCollaborator(docId, userId);
    
    this.eventBus.publish({
      type: COLLAB_EVENTS.DOCUMENT_COLLABORATOR_JOINED,
      payload: { docId, userId },
    });
  }
  
  async leaveDocument(docId: string, userId: string): Promise<void> {
    await this.docStore.removeCollaborator(docId, userId);
    
    this.eventBus.publish({
      type: COLLAB_EVENTS.DOCUMENT_COLLABORATOR_LEFT,
      payload: { docId, userId },
    });
  }
  
  async getDocumentCollaborators(docId: string): Promise<string[]> {
    return this.docStore.getActiveCollaborators(docId);
  }
  
  // ==================== WHITEBOARDS ====================
  
  async createWhiteboard(
    chatId: string,
    createdBy: string,
    name: string
  ): Promise<CollaborativeWhiteboard> {
    const wb = await this.wbStore.create({
      chatId,
      name,
      elements: [],
      participants: [createdBy],
      createdBy,
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    });
    
    this.eventBus.publish({
      type: COLLAB_EVENTS.WHITEBOARD_CREATED,
      payload: wb,
    });
    
    return wb;
  }
  
  async getWhiteboard(whiteboardId: string): Promise<CollaborativeWhiteboard | null> {
    return this.wbStore.get(whiteboardId);
  }
  
  async getChatWhiteboards(chatId: string): Promise<CollaborativeWhiteboard[]> {
    return this.wbStore.getByChat(chatId);
  }
  
  async addWhiteboardElement(
    whiteboardId: string,
    userId: string,
    data: Omit<WhiteboardElement, 'id' | 'createdBy' | 'createdAt'>
  ): Promise<WhiteboardElement> {
    const id = `elem-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const element: WhiteboardElement = {
      id,
      ...data,
      createdBy: userId,
      createdAt: new Date().toISOString(),
    };
    
    await this.wbStore.addElement(whiteboardId, element);
    await this.wbStore.recordAction(whiteboardId, {
      userId,
      actionType: 'add',
      elementId: id,
      timestamp: new Date().toISOString(),
    });
    
    this.eventBus.publish({
      type: COLLAB_EVENTS.WHITEBOARD_ELEMENT_ADDED,
      payload: { whiteboardId, element },
    });
    
    return element;
  }
  
  async updateWhiteboardElement(
    whiteboardId: string,
    elementId: string,
    userId: string,
    updates: Partial<WhiteboardElement>
  ): Promise<void> {
    await this.wbStore.updateElement(whiteboardId, elementId, updates);
    await this.wbStore.recordAction(whiteboardId, {
      userId,
      actionType: 'modify',
      elementId,
      data: updates,
      timestamp: new Date().toISOString(),
    });
    
    this.eventBus.publish({
      type: COLLAB_EVENTS.WHITEBOARD_ELEMENT_UPDATED,
      payload: { whiteboardId, elementId, updates },
    });
  }
  
  async removeWhiteboardElement(
    whiteboardId: string,
    elementId: string,
    userId: string
  ): Promise<void> {
    await this.wbStore.removeElement(whiteboardId, elementId);
    await this.wbStore.recordAction(whiteboardId, {
      userId,
      actionType: 'delete',
      elementId,
      timestamp: new Date().toISOString(),
    });
    
    this.eventBus.publish({
      type: COLLAB_EVENTS.WHITEBOARD_ELEMENT_REMOVED,
      payload: { whiteboardId, elementId },
    });
  }
  
  async getWhiteboardElements(whiteboardId: string): Promise<WhiteboardElement[]> {
    return this.wbStore.getElements(whiteboardId);
  }
}

// ============================================================================
// SINGLETON
// ============================================================================

let collaborationService: CollaborationService | null = null;

export function getCollaborationService(): CollaborationService {
  if (!collaborationService) {
    collaborationService = new CollaborationServiceImpl(
      new MemoryDocumentStore(),
      new MemoryWhiteboardStore(),
      getEventBus()
    );
  }
  return collaborationService;
}
