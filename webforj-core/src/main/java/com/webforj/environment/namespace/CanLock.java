package com.webforj.environment.namespace;

public interface CanLock {
  public void setLock(String key, long timeout) throws NamespaceVarableLockedException;

  public void removeLock(String key);
}
