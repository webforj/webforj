/**
 * Curated Vue plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
import vue from 'bun-plugin-vue3';

const vuePlugin = (options) => (typeof vue === 'function' ? vue(options) : vue);

export default vuePlugin;
