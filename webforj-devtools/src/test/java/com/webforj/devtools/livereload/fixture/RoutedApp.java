package com.webforj.devtools.livereload.fixture;

import com.webforj.App;
import com.webforj.annotation.Routify;

/**
 * An application that names the packages of its routes.
 */
@Routify(packages = {"com.webforj.devtools.livereload.fixture", "org.example.views"})
public class RoutedApp extends App {
}
