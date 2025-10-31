package com.webforj.minify.maven;

/**
 * Configuration options for the Closure Compiler JavaScript minifier.
 *
 * <p>This class provides configuration for the Google Closure Compiler used by the
 * webforj-minify-closure-js minifier. All options have sensible defaults that match the current
 * hardcoded behavior.
 *
 * <p>Example configuration in pom.xml:
 * <pre>{@code
 * <plugin>
 *   <groupId>com.webforj</groupId>
 *   <artifactId>webforj-minify-maven-plugin</artifactId>
 *   <configuration>
 *     <closureJsOptions>
 *       <compilationLevel>ADVANCED_OPTIMIZATIONS</compilationLevel>
 *       <languageIn>ECMASCRIPT_2020</languageIn>
 *       <languageOut>ECMASCRIPT5</languageOut>
 *       <prettyPrint>false</prettyPrint>
 *     </closureJsOptions>
 *   </configuration>
 *   <dependencies>
 *     <dependency>
 *       <groupId>com.webforj</groupId>
 *       <artifactId>webforj-minify-closure-js</artifactId>
 *       <version>${project.version}</version>
 *     </dependency>
 *   </dependencies>
 * </plugin>
 * }</pre>
 *
 * @author Kevin Hagel
 */
public class ClosureJsOptions {

  /**
   * Compilation level for optimization.
   *
   * <p>Valid values:
   * <ul>
   * <li><b>WHITESPACE_ONLY</b> - Only removes whitespace and comments</li>
   * <li><b>SIMPLE_OPTIMIZATIONS</b> - Renames local variables, removes dead code (default)</li>
   * <li><b>ADVANCED_OPTIMIZATIONS</b> - Aggressive optimization including function/property
   * renaming</li>
   * </ul>
   */
  private String compilationLevel = "SIMPLE_OPTIMIZATIONS";

  /**
   * Input JavaScript language version.
   *
   * <p>Valid values include:
   * <ul>
   * <li><b>ECMASCRIPT3</b> - ES3 (very old browsers)</li>
   * <li><b>ECMASCRIPT5</b> - ES5</li>
   * <li><b>ECMASCRIPT_2015</b> - ES6/ES2015</li>
   * <li><b>ECMASCRIPT_2016</b> - ES2016</li>
   * <li><b>ECMASCRIPT_2017</b> - ES2017</li>
   * <li><b>ECMASCRIPT_2018</b> - ES2018</li>
   * <li><b>ECMASCRIPT_2019</b> - ES2019</li>
   * <li><b>ECMASCRIPT_2020</b> - ES2020</li>
   * <li><b>ECMASCRIPT_2021</b> - ES2021</li>
   * <li><b>ECMASCRIPT_NEXT</b> - Latest supported features (default)</li>
   * </ul>
   */
  private String languageIn = "ECMASCRIPT_NEXT";

  /**
   * Output JavaScript language version for transpilation.
   *
   * <p>Valid values include:
   * <ul>
   * <li><b>ECMASCRIPT3</b> - ES3</li>
   * <li><b>ECMASCRIPT5</b> - ES5 (broad browser compatibility, default)</li>
   * <li><b>ECMASCRIPT_2015</b> - ES6/ES2015</li>
   * <li><b>ECMASCRIPT_2016</b> - ES2016</li>
   * <li><b>ECMASCRIPT_2017</b> - ES2017</li>
   * <li><b>ECMASCRIPT_2018</b> - ES2018</li>
   * <li><b>ECMASCRIPT_2019</b> - ES2019</li>
   * <li><b>ECMASCRIPT_2020</b> - ES2020</li>
   * <li><b>ECMASCRIPT_2021</b> - ES2021</li>
   * <li><b>NO_TRANSPILE</b> - No transpilation, output same version as input</li>
   * </ul>
   */
  private String languageOut = "ECMASCRIPT5";

  /**
   * Pretty print the output (preserve formatting for debugging).
   *
   * <p>When true, the output will be formatted with whitespace and line breaks. When false
   * (default), output will be minified to a single line.
   */
  private boolean prettyPrint = false;

  /**
   * Gets the compilation level.
   *
   * @return the compilation level
   */
  public String getCompilationLevel() {
    return compilationLevel;
  }

  /**
   * Sets the compilation level.
   *
   * @param compilationLevel the compilation level
   */
  public void setCompilationLevel(String compilationLevel) {
    this.compilationLevel = compilationLevel;
  }

  /**
   * Gets the input language version.
   *
   * @return the input language version
   */
  public String getLanguageIn() {
    return languageIn;
  }

  /**
   * Sets the input language version.
   *
   * @param languageIn the input language version
   */
  public void setLanguageIn(String languageIn) {
    this.languageIn = languageIn;
  }

  /**
   * Gets the output language version.
   *
   * @return the output language version
   */
  public String getLanguageOut() {
    return languageOut;
  }

  /**
   * Sets the output language version.
   *
   * @param languageOut the output language version
   */
  public void setLanguageOut(String languageOut) {
    this.languageOut = languageOut;
  }

  /**
   * Gets whether to pretty print the output.
   *
   * @return true if pretty print is enabled
   */
  public boolean isPrettyPrint() {
    return prettyPrint;
  }

  /**
   * Sets whether to pretty print the output.
   *
   * @param prettyPrint true to enable pretty print
   */
  public void setPrettyPrint(boolean prettyPrint) {
    this.prettyPrint = prettyPrint;
  }

  @Override
  public String toString() {
    return "ClosureJsOptions{"
        + "compilationLevel='" + compilationLevel + '\''
        + ", languageIn='" + languageIn + '\''
        + ", languageOut='" + languageOut + '\''
        + ", prettyPrint=" + prettyPrint
        + '}';
  }
}
