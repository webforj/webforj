package com.webforj.data.repository.spring;

import com.webforj.data.repository.AbstractQueryableRepository;
import com.webforj.data.repository.OrderCriteria;
import com.webforj.data.repository.OrderCriteriaList;
import com.webforj.data.repository.RepositoryCriteria;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.Repository;

/**
 * Bridge between webforj repositories and Spring Data repositories.
 *
 * <p>
 * This class wraps Spring Data repositories to provide webforj's unified repository interface with
 * full support for JPA Specifications as the filter type. It automatically detects which Spring
 * Data interfaces are implemented and provides corresponding functionality.
 * </p>
 *
 * <h3>Supported Spring Data Interfaces</h3>
 * <p>
 * The wrapped repository can implement any combination of these interfaces:
 * </p>
 * <ul>
 * <li>{@link CrudRepository} - Enables basic CRUD operations (find by ID, count)</li>
 * <li>{@link PagingAndSortingRepository} - Enables pagination and sorting without filters</li>
 * <li>{@link JpaSpecificationExecutor} - Enables JPA Specification queries with filtering,
 * pagination, and sorting</li>
 * </ul>
 *
 * <h3>Feature Compatibility by Interface Combination</h3>
 *
 * <h4>1. CrudRepository only</h4>
 * <ul>
 * <li>✓ {@code find(key)}</li>
 * <li>✗ {@code findBy()} without filter (throws UnsupportedOperationException)</li>
 * <li>✗ {@code findBy()} with filter (throws UnsupportedOperationException)</li>
 * <li>✓ {@code count()} without filter</li>
 * <li>✗ {@code count()} with filter (throws UnsupportedOperationException)</li>
 * </ul>
 *
 * <h4>2. PagingAndSortingRepository only</h4>
 * <ul>
 * <li>✗ {@code find(key)} (always returns empty)</li>
 * <li>✓ {@code findBy()} without filter (with pagination and sorting)</li>
 * <li>✗ {@code findBy()} with filter (throws UnsupportedOperationException)</li>
 * <li>✓ {@code count()} without filter</li>
 * <li>✗ {@code count()} with filter (throws UnsupportedOperationException)</li>
 * </ul>
 *
 * <h4>3. JpaSpecificationExecutor only</h4>
 * <ul>
 * <li>✗ {@code find(key)} (always returns empty)</li>
 * <li>✓ {@code findBy()} without filter (with limited pagination)</li>
 * <li>✓ {@code findBy()} with filter (with filtering, pagination, and sorting)</li>
 * <li>✓ {@code count()} without filter</li>
 * <li>✓ {@code count()} with filter</li>
 * </ul>
 *
 * <h4>4. CrudRepository + PagingAndSortingRepository</h4>
 * <ul>
 * <li>✓ {@code find(key)}</li>
 * <li>✓ {@code findBy()} without filter (with optimal pagination)</li>
 * <li>✗ {@code findBy()} with filter (throws UnsupportedOperationException)</li>
 * <li>✓ {@code count()} without filter</li>
 * <li>✗ {@code count()} with filter (throws UnsupportedOperationException)</li>
 * </ul>
 *
 * <h4>5. CrudRepository + JpaSpecificationExecutor</h4>
 * <ul>
 * <li>✓ {@code find(key)}</li>
 * <li>✓ {@code findBy()} without filter (with limited pagination)</li>
 * <li>✓ {@code findBy()} with filter</li>
 * <li>✓ {@code count()} without filter</li>
 * <li>✓ {@code count()} with filter</li>
 * </ul>
 *
 * <h4>6. All interfaces (recommended)</h4>
 * <ul>
 * <li>✓ {@code find(key)}</li>
 * <li>✓ {@code findBy()} without filter (with optimal pagination)</li>
 * <li>✓ {@code findBy()} with filter</li>
 * <li>✓ {@code count()} without filter</li>
 * <li>✓ {@code count()} with filter</li>
 * </ul>
 *
 * <h3>Recommended Approach for Full Features</h3>
 * <p>
 * For complete functionality with optimal performance, implement {@code JpaRepository<T, K>} and
 * {@code JpaSpecificationExecutor<T>} in your Spring Data repository:
 * </p>
 *
 * <pre>{@code @Repository
 * interface MyRepository
 *     extends JpaRepository<MyEntity, Long>, JpaSpecificationExecutor<MyEntity> {
 *   // JpaRepository provides CrudRepository + PagingAndSortingRepository
 *   // JpaSpecificationExecutor adds filtering capabilities
 * }
 * }</pre>
 * <p>
 * </p>
 *
 * <h3>Usage Examples</h3>
 *
 * <h4>Basic Repository (CrudRepository + PagingAndSortingRepository)</h4>
 *
 * <pre>{@code
 * &#64;Repository
 * interface CustomerRepository extends JpaRepository<Customer, Long> {
 * }
 *
 * // Create webforj repository
 * SpringDataRepository<Customer, Long> repo = new SpringDataRepository<>(customerRepository);
 *
 * // Find by ID
 * Optional<Customer> customer = repo.find(1L);
 *
 * // criteria with pagination and sorting
 * OrderCriteria<Customer, String> nameOrder =
 *     new OrderCriteria<>(Customer::getName, OrderCriteria.Direction.ASC, null, "name");
 * RepositoryCriteria<Customer, Specification<Customer>> criteria =
 *     new RepositoryCriteria<>(0L, 10L, new OrderCriteriaList<>(nameOrder), null);
 * List<Customer> customers = repo.findBy(criteria).toList();
 * }</pre>
 *
 * <h4>Advanced Repository with Specifications</h4>
 *
 * <pre>{@code
 * &#64;Repository
 * interface CustomerRepository
 *     extends JpaRepository<Customer, Long>, JpaSpecificationExecutor<Customer> {
 * }
 *
 * // Create webforj repository
 * SpringDataRepository<Customer, Long> repo = new SpringDataRepository<>(customerRepository);
 *
 * // Set base filter (always applied)
 * Specification<Customer> activeCustomers =
 *     (root, query, cb) -> cb.equal(root.get("active"), true);
 * repo.setBaseFilter(activeCustomers);
 *
 * // criteria with additional filter, pagination, and sorting
 * Specification<Customer> emailFilter =
 *     (root, query, cb) -> cb.like(root.get("email"), "%@company.com");
 * OrderCriteria<Customer, String> nameOrder =
 *     new OrderCriteria<>(Customer::getName, OrderCriteria.Direction.DESC, null, "name");
 * RepositoryCriteria<Customer, Specification<Customer>> criteria =
 *     new RepositoryCriteria<>(20L, 15L, new OrderCriteriaList<>(nameOrder), emailFilter);
 *
 * // This will find active customers with company emails,
 * // skip first 20, take 15, ordered by name descending
 * List<Customer> results = repo.findBy(criteria).toList();
 * int totalCount = repo.count(criteria);
 * }</pre>
 *
 * <h4>Composite Property Sorting</h4>
 *
 * <pre>{@code
 * // Table shows full name but needs to sort by firstName, lastName
 * table.addColumn("fullName", Customer::getFullName).setPropertyName("firstName,lastName");
 *
 * // The repository will automatically create multiple sort orders:
 * // ORDER BY firstName ASC, lastName ASC
 * }</pre>
 *
 * @param <T> Entity type
 * @param <K> Entity ID type
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 * @see CrudRepository
 * @see PagingAndSortingRepository
 * @see JpaSpecificationExecutor
 * @see RepositoryCriteria
 * @see Specification
 */
public class SpringDataRepository<T, K> extends AbstractQueryableRepository<T, Specification<T>> {

  private final CrudRepository<T, K> crudRepository;
  private final PagingAndSortingRepository<T, K> pagingRepository;
  private final JpaSpecificationExecutor<T> specificationExecutor;

  /**
   * Creates a webforj repository wrapping a Spring Data repository.
   *
   * <p>
   * The provided repository must implement at least one of the supported Spring Data interfaces.
   * The wrapper will automatically detect which interfaces are available and provide corresponding
   * functionality. See the class documentation for the complete feature matrix.
   * </p>
   *
   * <h4>Recommended Interface Combinations:</h4>
   * <ul>
   * <li><strong>Full-featured:</strong> {@code JpaRepository<T, K> + JpaSpecificationExecutor<T>}
   * <br>
   * Provides all functionality with optimal performance</li>
   * <li><strong>Basic:</strong> {@code JpaRepository<T, K>} <br>
   * Provides CRUD and pagination, but no filtering capabilities</li>
   * <li><strong>Query-only:</strong> {@code Repository<T, K> + JpaSpecificationExecutor<T>} <br>
   * Provides filtering and queries, but no CRUD operations</li>
   * </ul>
   *
   * @param <R> Spring Data repository type that extends {@link Repository}
   * @param springDataRepository Spring Data repository implementation. Must implement at least one
   *        of: {@link CrudRepository}, {@link PagingAndSortingRepository}, or
   *        {@link JpaSpecificationExecutor}
   * @throws NullPointerException if springDataRepository is null
   * @throws IllegalArgumentException if the repository doesn't implement any supported interfaces
   */
  @SuppressWarnings("unchecked")
  public <R extends Repository<T, K>> SpringDataRepository(R springDataRepository) {
    Objects.requireNonNull(springDataRepository, "Spring Data repository cannot be null");

    // Check and assign interfaces
    this.crudRepository =
        springDataRepository instanceof CrudRepository ? (CrudRepository<T, K>) springDataRepository
            : null;

    this.pagingRepository = springDataRepository instanceof PagingAndSortingRepository
        ? (PagingAndSortingRepository<T, K>) springDataRepository
        : null;

    this.specificationExecutor = springDataRepository instanceof JpaSpecificationExecutor
        ? (JpaSpecificationExecutor<T>) springDataRepository
        : null;

    // Ensure at least one interface is implemented
    if (crudRepository == null && pagingRepository == null && specificationExecutor == null) {
      throw new IllegalArgumentException(
          "Repository must implement at least one of: CrudRepository, "
              + "PagingAndSortingRepository, or JpaSpecificationExecutor");
    }
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * Finds an entity by its primary key. This method requires the wrapped repository to implement
   * {@link CrudRepository}.
   * </p>
   *
   * <h4>Behavior by Interface:</h4>
   * <ul>
   * <li><strong>With CrudRepository:</strong> Delegates to
   * {@code CrudRepository.findById(key)}</li>
   * <li><strong>Without CrudRepository:</strong> Always returns {@code Optional.empty()}</li>
   * </ul>
   *
   * @param key The primary key of the entity to find. Will be cast to type {@code K}
   * @return Optional containing the entity if found, empty otherwise
   * @throws ClassCastException if key cannot be cast to the entity's ID type {@code K}
   */
  @Override
  public Optional<T> find(Object key) {
    if (crudRepository == null) {
      return Optional.empty();
    }
    @SuppressWarnings("unchecked")
    K id = (K) key;
    return crudRepository.findById(id);
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * Executes a query with optional filtering, sorting, and pagination. The implementation strategy
   * depends on which interfaces the wrapped repository implements.
   * </p>
   *
   * @param query Query parameters including filter, sorting, and pagination. Must not be null.
   *        Filter can be null for unfiltered queries.
   * @return Stream of entities matching the query criteria
   * @throws NullPointerException if query is null
   * @throws UnsupportedOperationException if query has a filter but repository doesn't implement
   *         JpaSpecificationExecutor, or if pagination is requested but repository implements
   *         neither PagingAndSortingRepository nor JpaSpecificationExecutor
   * @throws ClassCastException if pagination parameters exceed integer range
   *
   * @see RepositoryCriteria
   * @see Specification
   * @see OrderCriteria
   */
  @Override
  public Stream<T> findBy(RepositoryCriteria<T, Specification<T>> query) {
    Objects.requireNonNull(query, "Query cannot be null");

    // Use only the query's filter, and ignore the base filter
    Specification<T> spec = query.getFilter();

    // If we have a specification but no executor, we can't handle it :(
    if (spec != null && specificationExecutor == null) {
      throw new UnsupportedOperationException(
          "Repository must implement JpaSpecificationExecutor to use Specification filters");
    }

    // Get offset and limit from query
    int offset = query.getOffset();
    int limit = query.getLimit();

    // Convert OrderCriteria to Spring Data Sort
    Sort sort = convertToSpringSort(query.getOrderCriteria());

    // Optimized pagination handling
    Page<T> result;
    PageRequest pageRequest;
    boolean needsSkipAndLimit = false;

    // Check if offset aligns with limit (common case)
    if (offset % limit == 0) {
      // Aligned case: can fetch exact page
      int page = offset / limit;
      pageRequest = PageRequest.of(page, limit, sort);
    } else {
      // Non-aligned case: Spring Data's page-based API doesn't map cleanly to offset/limit
      // The simplest correct approach is to fetch offset+limit items and skip offset
      pageRequest = PageRequest.of(0, offset + limit, sort);
      needsSkipAndLimit = true;
    }

    // Execute the query based on available interfaces
    if (spec != null) {
      // We have a specification, need JpaSpecificationExecutor
      if (specificationExecutor == null) {
        throw new UnsupportedOperationException(
            "Repository must implement JpaSpecificationExecutor to use Specification filters");
      }
      result = specificationExecutor.findAll(spec, pageRequest);
    } else {
      // No specification - try paging repository first, then specification executor
      if (pagingRepository != null) {
        result = pagingRepository.findAll(pageRequest);
      } else if (specificationExecutor != null) {
        result = specificationExecutor.findAll(null, pageRequest);
      } else {
        throw new UnsupportedOperationException(
            "Repository must implement PagingAndSortingRepository or JpaSpecificationExecutor "
                + "for pagination support");
      }
    }

    // Apply skip and limit for non-aligned cases
    if (needsSkipAndLimit) {
      return result.stream().skip(offset).limit(limit);
    } else {
      return result.stream();
    }
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * Counts entities matching the query criteria. The implementation strategy depends on whether a
   * filter is specified and which interfaces the wrapped repository implements.
   * </p>
   *
   * @param query Query parameters. Only the filter is used; pagination and sorting are ignored.
   *        Must not be null, but filter can be null for unfiltered counts.
   * @return Total number of entities matching the filter criteria
   * @throws NullPointerException if query is null
   * @throws UnsupportedOperationException if query has a filter but repository doesn't implement
   *         JpaSpecificationExecutor
   *
   * @see RepositoryCriteria
   * @see Specification
   */
  @Override
  public int size(RepositoryCriteria<T, Specification<T>> query) {
    Objects.requireNonNull(query, "Query cannot be null");

    // Use only the query's filter and ignore the base filter
    Specification<T> spec = query.getFilter();

    // If spec is null, use CrudRepository's count method
    if (spec == null && crudRepository != null) {
      return (int) crudRepository.count();
    }

    // Otherwise use specification executor
    if (specificationExecutor == null) {
      throw new UnsupportedOperationException(
          "Repository must implement JpaSpecificationExecutor for specification queries");
    }

    return (int) specificationExecutor.count(spec);
  }

  /**
   * Converts webforj OrderCriteria to Spring Data Sort.
   *
   * <p>
   * This method handles both simple and composite property names. Composite properties (separated
   * by commas) are expanded into multiple sort orders.
   * </p>
   *
   * @param orderCriteriaList List of order criteria to convert
   * @return Spring Data Sort object, or Sort.unsorted() if the list is empty
   */
  private Sort convertToSpringSort(OrderCriteriaList<T> orderCriteriaList) {
    if (orderCriteriaList.size() == 0) {
      return Sort.unsorted();
    }

    List<Sort.Order> orders = new ArrayList<>();
    for (OrderCriteria<T, ?> criteria : orderCriteriaList) {
      String propertyName = criteria.getPropertyName();
      if (propertyName != null) {
        // Handle composite properties separated by commas
        if (propertyName.contains(",")) {
          String[] properties = propertyName.split(",");
          for (String property : properties) {
            String trimmedProperty = property.trim();
            if (!trimmedProperty.isEmpty()) {
              Sort.Order order = criteria.getDirection() == OrderCriteria.Direction.ASC
                  ? Sort.Order.asc(trimmedProperty)
                  : Sort.Order.desc(trimmedProperty);
              orders.add(order);
            }
          }
        } else {
          // Single property
          Sort.Order order =
              criteria.getDirection() == OrderCriteria.Direction.ASC ? Sort.Order.asc(propertyName)
                  : Sort.Order.desc(propertyName);
          orders.add(order);
        }
      }
    }

    return orders.isEmpty() ? Sort.unsorted() : Sort.by(orders);
  }
}
