package org.dwcj.data.repository.event;

import java.util.EventObject;
import java.util.List;
import org.dwcj.data.repository.Repository;

/**
 * An event fired when a repository is refreshed.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class RepositoryCommitEvent<T> extends EventObject {
  private transient Repository<T> repository;
  private transient List<T> commits;

  /**
   * Creates a new instance of RepositoryCommitEvent.
   *
   * @param source The repository that fired the event.
   * @param commits The list of commits.
   */
  public RepositoryCommitEvent(Repository<T> source, List<T> commits) {
    super(source);
    this.repository = source;
    this.commits = commits;
  }

  /**
   * Gets the repository that fired the event.
   *
   * @return The repository that fired the event.
   */
  public Repository<T> getRepository() {
    return repository;
  }

  /**
   * Gets the list of commits.
   *
   * @return The list of commits.
   */
  public List<T> getCommits() {
    return commits;
  }
}
