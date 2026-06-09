/**
 * Curated LESS plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
import less from 'less';
import { readFileSync } from 'node:fs';

export default (options) => ({
  name: 'webforj-less',
  setup(build) {
    build.onLoad({ filter: /\.less$/ }, async (args) => {
      const result =
        await less.render(readFileSync(args.path, 'utf8'), {
          filename: args.path,
          ...options
        });

      return { contents: result.css, loader: 'css' };
    });
  }
});
