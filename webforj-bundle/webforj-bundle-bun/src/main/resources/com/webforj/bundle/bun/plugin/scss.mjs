/**
 * Curated SCSS/Sass plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
import { compile } from 'sass';

export default (options) => ({
  name: 'webforj-scss',
  setup(build) {
    build.onLoad({ filter: /\.s[ac]ss$/ }, (args) => ({
      contents: compile(args.path, options).css,
      loader: 'css'
    }));
  }
});
