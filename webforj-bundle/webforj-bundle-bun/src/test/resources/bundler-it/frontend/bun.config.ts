const textPlugin = {
  name: 'it-text',
  setup(build) {
    build.onLoad({ filter: /\.txt$/ }, async (args) => ({
      contents: 'export default ' + JSON.stringify(await Bun.file(args.path).text()) + ';',
      loader: 'js'
    }));
  }
};

export default [textPlugin];
