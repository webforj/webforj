package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.source.SourceFileEditor;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.TargetResolver;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.site.ComponentSiteResolver;
import com.webforj.devtools.craftforj.source.staging.StagingException;
import com.webforj.devtools.craftforj.source.structure.StructureModifier;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import java.io.IOException;
import java.util.List;
import java.util.function.Function;

/**
 * Removes a component from the Java source that creates it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class RemoveComponentSourceAction
    implements CraftforjActionHandler<RemoveComponentSourceAction.Response> {

  /** Action name. */
  public static final String ACTION = "inspector.removeComponentSource";

  private static final String SOURCE_KEY = "source";
  private static final Gson GSON = new Gson();
  private final StructureModifier editor;
  private final ComponentSiteResolver siteResolver;
  private final TargetResolver targetResolver;
  private final Function<String, Component> componentFinder;

  /**
   * Creates the action with the current parser service.
   */
  public RemoveComponentSourceAction() {
    this(
        new StructureModifier(new SourceFileEditor(SourceParserService.getCurrent()),
            SourceParserService.getCurrent()),
        new ComponentSiteResolver(SourceParserService.getCurrent()),
        id -> ComponentLocator.findById(id).orElse(null));
  }

  /**
   * Creates the action.
   *
   * @param editor the editor that takes the component out of the source
   * @param siteResolver the resolver of the expression that stands for a component
   * @param componentFinder the runtime component lookup
   */
  RemoveComponentSourceAction(StructureModifier editor, ComponentSiteResolver siteResolver,
      Function<String, Component> componentFinder) {
    this.editor = editor;
    this.siteResolver = siteResolver;
    this.targetResolver = new TargetResolver(SourceParserService.getCurrent());
    this.componentFinder = componentFinder;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Response handle(JsonObject params) {
    String componentId = readString(params, "componentId");
    try {
      Component component = componentId == null ? null : componentFinder.apply(componentId);
      List<SourceLocation> components =
          component == null ? List.of(readStored(params)) : siteResolver.resolveTree(component);
      List<FilePatch> patches = editor.remove(components, null, false);

      return patches.isEmpty() ? Response.removed(components.get(0).getFile(), false)
          : Response.removed(patches.get(0).getFile(), true);
    } catch (SourceModificationException | StagingException | IOException e) {
      return Response.refused(e.getMessage());
    }
  }

  // A component that is gone left no trace of the expression that created it, only a variable
  // can still be found by its name
  private SourceLocation readStored(JsonObject params) {
    SourceLocation stored = params.has(SOURCE_KEY) && params.get(SOURCE_KEY).isJsonObject()
        ? GSON.fromJson(params.get(SOURCE_KEY), SourceLocation.class)
        : null;
    SourceLocation location = targetResolver.resolve(null, stored);
    if (location == null) {
      throw new SourceModificationException("The source of this component was not found");
    }

    String name = location.getVariableName();
    if (location.getSite() == null && (name == null || name.isBlank())) {
      throw new SourceModificationException(
          "This component is no longer part of the running application, reload it first");
    }

    if (location.getComponentType() == null) {
      location.setComponentType(readString(params, "componentType"));
    }

    if (location.getComponentType() == null || location.getComponentType().isBlank()) {
      throw new SourceModificationException("The type of this component is unknown");
    }

    return location;
  }

  private static String readString(JsonObject params, String name) {
    return params.has(name) && params.get(name).isJsonPrimitive() ? params.get(name).getAsString()
        : null;
  }

  /**
   * The outcome of a removal.
   */
  public static class Response {

    private final boolean removed;
    private final String file;
    private final String message;

    private Response(boolean removed, String file, String message) {
      this.removed = removed;
      this.file = file;
      this.message = message;
    }

    /**
     * Checks whether the component left the source.
     *
     * @return {@code true} when the file was written
     */
    public boolean isRemoved() {
      return removed;
    }

    /**
     * Gets the file the component was removed from.
     *
     * @return the absolute path, or {@code null} when refused
     */
    public String getFile() {
      return file;
    }

    /**
     * Gets why nothing was removed.
     *
     * @return the reason, or {@code null} when removed
     */
    public String getMessage() {
      return message;
    }

    static Response removed(String file, boolean changed) {
      return new Response(changed, file, changed ? null : "Nothing was removed");
    }

    static Response refused(String message) {
      return new Response(false, null, message);
    }
  }
}
