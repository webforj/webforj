/**
 * Curated Svelte plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
import { SveltePlugin } from 'bun-plugin-svelte';

export default (options) => SveltePlugin({ forceSide: 'client', ...options });
