/**
 * Asynchronous access to browser media queries and their change notifications.
 *
 * <pre>{@code
 * MediaQuery.getCurrent().matchMedia("(max-width: 700px)").thenAccept(query -> {
 *   query.onChange(event -> {
 *     boolean narrow = event.isMatched();
 *     // Update the UI using the snapshot carried by the event.
 *   }, true); // Include an initial notification for this listener.
 * });
 * }</pre>
 *
 * <p>
 * Omitting the boolean argument delivers changes only. The initial notification is asynchronous and
 * can be distinguished with {@link com.webforj.matchmedia.event.MediaQueryChangeEvent#isInitial()}. A
 * registration's {@link com.webforj.matchmedia.MediaQueryListenerRegistration#whenReady()} result
 * reports browser installation failures. Call
 * {@link com.webforj.matchmedia.MediaQueryList#destroy()} when a view no longer needs its query;
 * application termination also disposes remaining queries.
 * </p>
 *
 * @since 26.03
 */
package com.webforj.matchmedia;
