package com.webforj.data.concern;

import java.util.function.Function;

/**
 * An interface for components and repositories that support custom key provider functions for
 * extracting entity identifiers.
 *
 * <p>
 * This interface provides methods to set and retrieve a key provider function that extracts unique
 * identifiers from entities. Key providers are essential for components like tables that need to
 * track entities by their IDs for client-server communication and state management.
 * </p>
 *
 * <h3>Key Extraction Priority</h3>
 * <p>
 * When a key provider is set, it typically follows this priority chain:
 * </p>
 * <ol>
 * <li>Custom key provider (set via {@link #setKeyProvider})</li>
 * <li>{@link com.webforj.data.HasEntityKey} interface (if entity implements it)</li>
 * <li>Implementation-specific default (e.g., entity itself for backward compatibility)</li>
 * </ol>
 *
 * <h3>Usage Examples</h3>
 *
 * <h4>Setting a Key Provider on a Repository</h4>
 *
 * <pre>{@code
 * Repository<Customer> repository = ...;
 * repository.setKeyProvider(Customer::getId);
 * }</pre>
 *
 * <h4>Using with JPA Entities</h4>
 *
 * <pre>
 * {@code
 * &#64;Entity
 * public class Product {
 *   &#64;Id
 *   private Long id;
 *   // ... other fields
 * }
 *
 * Repository<Product> repository = ...;
 * repository.setKeyProvider(Product::getId);
 * }
 * </pre>
 *
 * @param <T> the entity type
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public interface HasKeyProvider<T> {

  /**
   * Sets the key provider function for extracting entity identifiers.
   *
   * <p>
   * The key provider is a function that takes an entity and returns its unique identifier. This is
   * particularly useful when working with JPA entities where you want to use the entity's database
   * primary key as the identifier for client-side tracking.
   * </p>
   *
   * <p>
   * <strong>Note:</strong> Implementations may trigger side effects when the key provider is
   * changed, such as notifying listeners or clearing cached keys to ensure consistency.
   * </p>
   *
   * @param keyProvider the function to extract keys from entities. Must not be null.
   * @return the component itself for method chaining
   * @throws NullPointerException if keyProvider is null
   */
  HasKeyProvider<T> setKeyProvider(Function<T, ?> keyProvider);

  /**
   * Gets the current key provider function.
   *
   * <p>
   * The returned function represents the current strategy for extracting entity keys. This may be a
   * custom provider set via {@link #setKeyProvider}, or a default provider supplied by the
   * implementation.
   * </p>
   *
   * @return the key provider function, never null
   */
  Function<T, ?> getKeyProvider();
}
