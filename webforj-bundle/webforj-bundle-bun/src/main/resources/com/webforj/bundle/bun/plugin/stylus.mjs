/**
 * Curated Stylus plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
import stylus from 'stylus';
import { readFileSync } from 'fs';

export default (options) => ({
  name: 'webforj-stylus',
  setup(build) {
    build.onLoad({ filter: /\.styl$/ }, (args) => ({
      contents: stylus.render(readFileSync(args.path, 'utf8'), {
        filename: args.path,
        ...options
      }),
      loader: 'css'
    }));
  }
});
