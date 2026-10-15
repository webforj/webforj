package com.webforj.devtools.livereload.fixture;

import com.webforj.App;
import com.webforj.annotation.Routify;

/**
 * An application whose routes live in packages outside the fixtures.
 */
@Routify(packages = "org.example.views")
public class ForeignRoutesApp extends App {
}
