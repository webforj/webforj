/**
 * Curated Svelte plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
import { SveltePlugin } from 'bun-plugin-svelte';

const sveltePlugin = (options) => SveltePlugin({ forceSide: 'client', ...options });

export default sveltePlugin;
