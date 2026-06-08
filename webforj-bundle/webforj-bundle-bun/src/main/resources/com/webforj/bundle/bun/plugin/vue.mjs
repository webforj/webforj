/**
 * Curated Vue plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
import vue from 'bun-plugin-vue3';

export default (options) => (typeof vue === 'function' ? vue(options) : vue);
