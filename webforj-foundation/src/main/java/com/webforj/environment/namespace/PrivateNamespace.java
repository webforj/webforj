package com.webforj.environment.namespace;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.NoSuchElementException;
import java.util.function.Consumer;

/**
 * A private namespace is shared between all clients that know its prefix and name.
 *
 * @since 24.22
 * @author Stephen Wald
 * @author Hyyan Abo Fakher
 */
public final class PrivateNamespace extends Namespace {

  /**
   * Retrieves a private namespace with the given prefix and name.
   *
   * <p>
   * The name of a namespace consists of two parts: the <code>prefix</code> and the
   * <code>name</code>. The full name of a namespace is <code>prefix + "." + name</code>. The prefix
   * and the suffix may be any non-empty string consisting of printable characters that begins with
   * a letter and contains no white space. A prefix that is not likely to be used by others should
   * be used.
   * </p>
   *
   * @param prefix This name is prefixed to <code>name</code> to create the actual name of a
   *        namespace.
   * @param name This name is suffixed to <code>prefix</code> to create the actual name of a
   *        namespace.
   * @param createIfMissing Indication of whether a new Variable Domain should be created if one
   *        does not already exist. If <code>true</code>, a new Variable Domain will be created if
   *        one does not already exist. If <code>false</code>, a <code>NoSuchElementException</code>
   *        will be thrown if a Variable Domain does not already exist.
   *
   * @throws NoSuchElementException if the namespace does not exist
   * @throws WebforjRuntimeException if the namespace cannot be created
   */
  public PrivateNamespace(String prefix, String name, Boolean createIfMissing) {
    try {
      setBbjNamespace(
          Environment.getCurrent().getBBjAPI().getNamespace(prefix, name, createIfMissing));
    } catch (BBjException e) {
      if (Boolean.TRUE.equals(createIfMissing)) {
        throw new WebforjRuntimeException(e);
      } else {
        throw new NoSuchElementException(e);
      }
    }
  }

  /**
   * Retrieves a private namespace with the given prefix and name.
   *
   * @param prefix This name is prefixed to <code>name</code> to create the actual name of a
   *        namespace.
   * @param name This name is suffixed to <code>prefix</code> to create the actual name of a
   *        namespace.
   *
   * @throws NoSuchElementException if the namespace does not exist
   * @throws WebforjRuntimeException if the namespace cannot be created
   */
  public PrivateNamespace(String prefix, String name) {
    this(prefix, name, true);
  }

  /**
   * Retrieves a private namespace with the given prefix.
   *
   * @param prefix This name is prefixed to <code>name</code> to create the actual name of a
   *        namespace.
   *
   * @throws WebforjRuntimeException if the namespace cannot be created
   */
  public PrivateNamespace(String prefix) {
    try {
      setBbjNamespace(Environment.getCurrent().getBBjAPI().getNewNamespace(prefix));
    } catch (BBjException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  private PrivateNamespace(BBjNamespace bbjNamespace) {
    setBbjNamespace(bbjNamespace);
  }

  /**
   * Retrieves an existing private namespace with the given name.
   *
   * @param name Specifies the name of the Namespace to be referenced.
   * @return The Namespace with the specified name.
   * @throws NoSuchElementException if the namespace does not exist
   */
  public static PrivateNamespace ofExisting(String name) {
    try {
      return new PrivateNamespace(Environment.getCurrent().getBBjAPI().getExistingNamespace(name));
    } catch (BBjException e) {
      throw new NoSuchElementException(e);
    }
  }

  /**
   * Checks if a namespace with the given name exists.
   *
   * @param name Specifies the name of the Namespace to be referenced.
   * @return {@code true} if the namespace exists, {@code false} otherwise.
   */
  public static boolean isPresent(String name) {
    try {
      PrivateNamespace.ofExisting(name);
      return true;
    } catch (NoSuchElementException e) {
      return false;
    }
  }

  /**
   * Checks if a namespace with the given name exists and executes the given action if it does.
   *
   * @param name Specifies the name of the Namespace to be referenced.
   * @param action The action to be executed if the namespace exists.
   */
  public static void ifPresent(String name, Consumer<PrivateNamespace> action) {
    try {
      if (isPresent(name)) {
        PrivateNamespace namespace = PrivateNamespace.ofExisting(name);
        action.accept(namespace);
      }
    } catch (NoSuchElementException e) {
      // pass
    }
  }
}
