/**
 * webforJ View Transitions - Client-side coordination script
 *
 * Handles the view transition lifecycle between server and client.
 *
 * @since 25.11
 */
(function () {
  'use strict';

  if (window.__webforjViewTransitions) return;

  const READY_ATTR = 'data-webforj-vt-ready';

  const VT = {
    pending: new Map(),
    directResolve: null,

    /**
     * Dispatch event to server
     *
     * @param {string} action - The action type of the event
     * @param {string} transitionId - Unique transition identifier
     */
    dispatchEvent(action, transitionId) {
      const event = new CustomEvent('vt-event', {
        bubbles: true,
        detail: { action, transitionId }
      });
      document.body.dispatchEvent(event);
    },

    /**
     * Execute update callback on server and wait for completion
     *
     * @param {string} transitionId - Unique transition identifier
     */
    async executeUpdate(transitionId) {
      return new Promise(resolve => {
        this.pending.set(transitionId + '-update', resolve);
        this.dispatchEvent('update', transitionId);
      });
    },

    /**
     * Notify server that transition is ready
     *
     * @param {string} transitionId - Unique transition identifier
     */
    executeReady(transitionId) {
      this.dispatchEvent('ready', transitionId);
    },

    /**
     * Notify server that transition is complete
     *
     * @param {string} transitionId - Unique transition identifier
     */
    complete(transitionId) {
      this.dispatchEvent('complete', transitionId);
    },

    /**
     * Resolve the update promise (called by server)
     *
     * @param {string} transitionId - Unique transition identifier
     */
    resolveUpdate(transitionId) {
      const resolve = this.pending.get(transitionId + '-update');
      if (resolve) {
        this.pending.delete(transitionId + '-update');
        resolve();
      }
    },

    /**
     * Request server to prepare exit styles, returns promise resolved when styles are applied
     *
     * @param {string} transitionId - Unique transition identifier
     */
    async prepareExit(transitionId) {
      return new Promise(resolve => {
        this.pending.set(transitionId + '-prepare', resolve);
        this.dispatchEvent('prepare', transitionId);
      });
    },

    /**
     * Called by server when exit styles are applied
     *
     * @param {string} transitionId - Unique transition identifier
     */
    resolvePrepare(transitionId) {
      const resolve = this.pending.get(transitionId + '-prepare');
      if (resolve) {
        this.pending.delete(transitionId + '-prepare');
        resolve();
      }
    },

    /**
     * Start a view transition
     *
     * @param {string} transitionId - Unique transition identifier
     */
    async startTransition(transitionId) {
      // Check for View Transitions API support
      if (!document.startViewTransition) {
        await this.executeUpdate(transitionId);
        this.complete(transitionId);
        return;
      }

      // Ask server to apply exit styles, wait for confirmation
      await this.prepareExit(transitionId);

      // Start the view transition - exit styles are now guaranteed in DOM
      const transition = document.startViewTransition(async () => {
        // Execute server-side update (DOM mutations, navigation)
        await this.executeUpdate(transitionId);
      });

      // Handle ready state
      transition.ready
        .then(() => this.executeReady(transitionId))
        .catch(() => { });

      // Handle completion
      transition.finished
        .then(() => this.complete(transitionId))
        .catch(() => this.complete(transitionId));
    }
  };

  window.__webforjViewTransitions = VT;
})();
