package org.example;

import com.webforj.App;
import com.webforj.annotation.AppProfile;
import com.webforj.annotation.AppTitle;
import com.webforj.annotation.Routify;
import com.webforj.annotation.StyleSheet;

/**
 * A simple HelloWorld app.
 */
@Routify(packages = "org.example.views")
// @AppTitle("webforJ Hello World")
@StyleSheet("ws://app.css")
@AppProfile(name = "my-tests", shortName = "my-tests")
public class Application extends App {
}
