package com.webforj.bundle.bun.discovery;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * One bundle entry a class declares through {@code @BundleEntry}, carried through the build.
 *
 * <p>
 * {@code source} is the declared path, which is also the key the runtime resolves the class by. An
 * entry is either a local source file under the bundle source root, or a bare npm specifier (for
 * example {@code @ui5/webcomponents/dist/Button.js}) that Bun resolves from the installed
 * node_modules, which lets a project consume a package with no source of its own.
 * </p>
 *
 * <p>
 * The {@code owners} are the routed classes whose output is the entry. An entry with owners loads
 * for those classes, and an entry with no owners loads for every view.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class BundleEntryDeclaration {
  private String source = null;
  private Path file = null;
  private boolean npm = false;
  private String buildPath = null;
  private List<String> owners = new ArrayList<>();

  /**
   * Sets the declared entry path, the value of {@code @BundleEntry}, which is also the index key.
   *
   * @param source the declared entry path
   * @return the declaration
   */
  public BundleEntryDeclaration setSource(String source) {
    this.source = source;

    return this;
  }

  /**
   * Gets the declared entry path, the value of {@code @BundleEntry}, which is also the index key.
   *
   * @return the declared entry path
   * @see #setSource(String)
   */
  public String getSource() {
    return source;
  }

  /**
   * Sets the resolved local source file, or null for an npm specifier.
   *
   * @param file the resolved local source file
   * @return the declaration
   */
  public BundleEntryDeclaration setResolvedFile(Path file) {
    this.file = file;

    return this;
  }

  /**
   * Gets the resolved local source file, or null for an npm specifier.
   *
   * @return the resolved local source file
   * @see #setResolvedFile(Path)
   */
  public Path getResolvedFile() {
    return file;
  }

  /**
   * Sets whether the entry is an npm specifier rather than a local file.
   *
   * @param npm {@code true} when the entry is an npm specifier
   * @return the declaration
   */
  public BundleEntryDeclaration setNpm(boolean npm) {
    this.npm = npm;

    return this;
  }

  /**
   * Indicates whether the entry is an npm specifier rather than a local file.
   *
   * @return {@code true} when the entry is an npm specifier
   * @see #setNpm(boolean)
   */
  public boolean isNpm() {
    return npm;
  }

  /**
   * Sets the path Bun compiles for this entry, relative to the bundle source root, a local source
   * or a generated synthetic file that imports an npm specifier.
   *
   * @param buildPath the path Bun compiles
   * @return the declaration
   */
  public BundleEntryDeclaration setBuildPath(String buildPath) {
    this.buildPath = buildPath;

    return this;
  }

  /**
   * Gets the path Bun compiles for this entry, relative to the bundle source root.
   *
   * @return the path Bun compiles
   * @see #setBuildPath(String)
   */
  public String getBuildPath() {
    return buildPath;
  }

  /**
   * Sets the routed classes the entry loads for. An empty list loads the entry for every view.
   *
   * @param owners the fully qualified names of the routed classes, never null
   * @return the declaration
   */
  public BundleEntryDeclaration setOwners(List<String> owners) {
    this.owners = owners == null ? new ArrayList<>() : new ArrayList<>(owners);

    return this;
  }

  /**
   * Adds one routed class the entry loads for.
   *
   * @param owner the fully qualified name of the routed class
   * @return the declaration
   */
  public BundleEntryDeclaration addOwner(String owner) {
    owners.add(owner);

    return this;
  }

  /**
   * Gets the routed classes the entry loads for, never null. An empty list loads the entry for
   * every view.
   *
   * @return the routed classes the entry loads for
   * @see #setOwners(List)
   */
  public List<String> getOwners() {
    return owners;
  }
}
