package com.webforj.bundle.bun;

import com.webforj.bundle.bun.discovery.BundleEntryResolver;
import com.webforj.bundle.bun.discovery.ClasspathPackageScanner;
import com.webforj.bundle.bun.runtime.BunRuntime;
import com.webforj.bundle.bun.writer.BundleIndexWriter;
import com.webforj.bundle.bun.writer.BundleDriverWriter;
import com.webforj.bundle.bun.writer.PackageJsonWriter;

/**
 * Builds a {@link BundlerExecution} from its collaborators.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundlerExecutionBuilder {

  ClasspathPackageScanner scanner = null;
  BundleEntryResolver resolver = null;
  PackageJsonWriter packageJson = null;
  BundleIndexWriter indexWriter = null;
  BundleDriverWriter driverWriter = null;
  BunRuntime bunRuntime = null;

  /**
   * Sets the classpath scanner used to find {@code @BundlePackage} and {@code @BundleEntry}.
   *
   * @param scanner the classpath scanner
   * @return the builder
   */
  public BundlerExecutionBuilder setScanner(ClasspathPackageScanner scanner) {
    this.scanner = scanner;

    return this;
  }

  /**
   * Sets the entry resolver used against the bundle source root.
   *
   * @param resolver the entry resolver
   * @return the builder
   */
  public BundlerExecutionBuilder setResolver(BundleEntryResolver resolver) {
    this.resolver = resolver;

    return this;
  }

  /**
   * Sets the package.json writer.
   *
   * @param packageJsonWriter the package.json writer
   * @return the builder
   */
  public BundlerExecutionBuilder setPackageJsonWriter(PackageJsonWriter packageJsonWriter) {
    this.packageJson = packageJsonWriter;

    return this;
  }

  /**
   * Sets the index writer used to produce the runtime classpath index.
   *
   * @param indexWriter the index writer
   * @return the builder
   */
  public BundlerExecutionBuilder setIndexWriter(BundleIndexWriter indexWriter) {
    this.indexWriter = indexWriter;

    return this;
  }

  /**
   * Sets the build driver writer.
   *
   * @param driverWriter the build driver writer
   * @return the builder
   */
  public BundlerExecutionBuilder setDriverWriter(BundleDriverWriter driverWriter) {
    this.driverWriter = driverWriter;

    return this;
  }

  /**
   * Sets the resolved Bun runtime.
   *
   * @param bunRuntime the resolved Bun runtime
   * @return the builder
   */
  public BundlerExecutionBuilder setBunRuntime(BunRuntime bunRuntime) {
    this.bunRuntime = bunRuntime;

    return this;
  }

  /**
   * Builds the execution.
   *
   * @return a new execution
   */
  public BundlerExecution build() {
    return new BundlerExecution(this);
  }
}
