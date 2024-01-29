/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable max-classes-per-file */
import * as PIXI from 'pixi.js';
import { utils } from 'pixi.js';
import '@pixi/graphics-extras';
import { PixiCorePrimitive } from './pixiCorePrimitive';
import { ScreenVector } from '../math/screenvector';
import {
  CullStatus,
  ICurvedBackgroundPool,
  ILinesPool,
  IPrimitive,
  IPrimitiveFactory,
  IRenderer,
  IShapesPool,
  IStraightBackgroundPool,
  ITickDelegate,
} from './renderer';
import { PixiRenderGroup } from './pixirendergroup';
import { lerp, limitNumberTo } from '../math/mathutilities';
import { rendererDefaults } from './rendererDefaults';
import { PixiBitmapFont } from './pixiBitmapFont';
import { CallbackQueue, QueuedCallback } from '../callbackqueue';
import { PixiPrimitiveFactorySdf } from './pixiPrimitiveFactorySdf';
import { MockPrimitiveFactory } from './mockPrimitiveFactory';
import { PixiPrimitiveFactoryInstanced } from './pixiPrimitiveFactoryInstanced';
import { PixiPrimitiveFactory } from './pixiPrimitiveFactory';
import { ShaderMap } from './shaderMap';
import { CanceledOperation, PixiAssetsWrapper } from './pixiAssetsWrapper';
import { PixiShapesPool } from './pixiShapesPool';
import { PixiLinesPool } from './pixiLinesPool';
import { PixiCurvedBackgroundPool } from './pixiCurvedBackgroundPool';
import { PixiStraightBackgroundPool } from './pixiStraightBackgroundPool';

export interface IPixiParams {
  backGroundColor?: number;
  defaultTransitionTime?: number;
  fullscreen?: boolean;
  height: number;
  width: number;
}

const defaultConfig: IPixiParams = {
  defaultTransitionTime: 0.5,
  fullscreen: true,
  height: 840,
  width: 1480,
};

enum PixiFactoryType {
  Conventional,
  SDF,
  InstancedSDF,
  Mockup,
}

/*
  Hack method to make sure all respurces by PIXI are properly disposed
*/
export function cleanPixiRendererMemory(renderer: any) {
  const { buffer, texture, CONTEXT_UID } = renderer;
  const { BaseTextureCache, TextureCache, ProgramCache } = utils;
  const disposeContextTexture = disposeTexture.bind(null, CONTEXT_UID);

  // Clear all WebGL allocated buffers
  buffer.disposeAll(true);

  // Clear all WebGL allocated textures
  texture.boundTextures.forEach(disposeContextTexture);
  texture.boundTextures.length = 0;
  texture.managedTextures.forEach(disposeContextTexture);
  texture.managedTextures.length = 0;

  // Clear GLTexture from singleton cache for current webgl context
  // eslint-disable-next-line no-restricted-syntax, guard-for-in
  for (const key in TextureCache) {
    disposeContextTexture(TextureCache[key]?.baseTexture);
  }

  // Clear GLTexture from singleton cache for current webgl context
  // eslint-disable-next-line no-restricted-syntax, guard-for-in
  for (const key in BaseTextureCache) {
    disposeContextTexture(BaseTextureCache[key]);
  }

  // Clear GLPrograms from singleton cache for current webgl context
  // eslint-disable-next-line no-restricted-syntax, guard-for-in
  for (const key in ProgramCache) {
    removeGlProgram(CONTEXT_UID, ProgramCache[key]);
  }
}

export function disposeTexture(ctx_uid: any, tex: any) {
  if (tex) {
    tex.dispose();
    tex.removeAllListeners();
    removeGlTexture(ctx_uid, tex);
  }
}

export function removeGlTexture(ctx_id: any, tex: any) {
  // eslint-disable-next-line no-param-reassign, @typescript-eslint/no-unused-expressions, no-underscore-dangle
  tex && delete tex._glTextures[ctx_id];
}

export function removeGlProgram(ctx_id: any, program: any) {
  // eslint-disable-next-line no-param-reassign, @typescript-eslint/no-unused-expressions, no-underscore-dangle
  program && delete program.glPrograms[ctx_id];
}

export class PixiRenderer implements IRenderer {
  private app: PIXI.Application;
  private root: PixiRenderGroup | null = null;

  private bReleased = false;
  private bCriticalsLoaded = false;
  private bShadersLoaded = false;
  private bResize = false;
  private totalTimeSeconds = 0;

  private mTickDelegate: ITickDelegate | undefined;

  private readonly onLoadCriticalsCallbacks = new CallbackQueue<IRenderer>();

  private mMinZoom = rendererDefaults.minZoom;
  private mMaxZoom = rendererDefaults.maxZoom;

  private mParentDivElement: HTMLDivElement | null = null;
  private mParentDivResizeObserver: ResizeObserver | null = null;

  private minimumTickLenght = 0.016666;
  private accumulatedTickTime = 0;

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private callWindowResize = (event: any) => {
    this.onWindowResize();
  };

  private fontProgram: PIXI.Program | null = null;
  private curvedBackgroundProgram: PIXI.Program | null = null;
  private straightBackgroundProgram: PIXI.Program | null = null;
  private lineProgram: PIXI.Program | null = null;
  private sdfShapeProgram: PIXI.Program | null = null;
  private instFontProgram: PIXI.Program | null = null;
  private instCurvedBackgroundProgram: PIXI.Program | null = null;
  private instStraightBackgroundProgram: PIXI.Program | null = null;
  private instLineProgram: PIXI.Program | null = null;
  private instSdfShapeProgram: PIXI.Program | null = null;
  private decalGeometry: PIXI.Geometry | null = null;

  private mPrimitiveFactory: IPrimitiveFactory = new MockPrimitiveFactory();

  private mShapesPool: IShapesPool | null = null;
  private mLinesPool: ILinesPool | null = null;
  private mCurvedBackgroundPool: ICurvedBackgroundPool | null = null;
  private mStraightBackgroundPool: IStraightBackgroundPool | null = null;

  private readonly fontsMap = new Map<string, PixiBitmapFont>();
  private readonly bitmaps = new Map<string, PIXI.Texture<PIXI.Resource>>();

  private readonly shaderMap = new ShaderMap(rendererDefaults.shadersBasePath);

  private readonly assetLoader = new PixiAssetsWrapper();

  // Updated every accumulated tick
  private readonly globalUniforms = {
    deltaTime: 0.1,
    totalTime: 0,

    // Screen size, in CSS pixels
    screenWidth: 1480,
    screenHeight: 840,

    // HTML canvas element size, in device (not CSS) pixels
    viewportWidth: 1480,
    viewportHeight: 840,

    screenPixelRatio: 1,
    screenScale: 1.0,
  };

  readonly rootMaxBounds = new ScreenVector(Number.MAX_VALUE, Number.MAX_VALUE);
  readonly rootMinBounds = new ScreenVector(
    -Number.MAX_VALUE,
    -Number.MAX_VALUE
  );

  readonly onResize: CallbackQueue<void> = new CallbackQueue<void>();

  set parentDivElement(divElement: HTMLDivElement | null) {
    this.mParentDivElement = divElement;

    if (this.mParentDivElement) {
      this.onWindowResize();

      this.mParentDivResizeObserver = new ResizeObserver(() => {
        this.onWindowResize();
        this.bResize = true;
      });

      this.mParentDivResizeObserver.observe(this.mParentDivElement);
    }
  }

  get viewCanvas(): HTMLCanvasElement {
    return this.app.view;
  }

  get screenWidth(): number {
    return this.app.screen.width;
  }

  get screenHeight(): number {
    return this.app.screen.height;
  }

  get smallerDimension(): number {
    return Math.min(this.screenWidth, this.screenHeight);
  }

  get minZoom(): number {
    return this.mMinZoom;
  }

  set minZoom(value: number) {
    this.mMinZoom = value;
  }

  get maxZoom(): number {
    return this.mMaxZoom;
  }

  set maxZoom(value: number) {
    this.mMaxZoom = value;
  }

  get primitiveFactory(): IPrimitiveFactory {
    return this.mPrimitiveFactory;
  }

  get shapesPool(): IShapesPool | null {
    return this.mShapesPool;
  }

  get linesPool(): ILinesPool | null {
    return this.mLinesPool;
  }

  get curvedBackgroundPool(): ICurvedBackgroundPool | null {
    return this.mCurvedBackgroundPool;
  }

  get straightBackgroundPool(): IStraightBackgroundPool | null {
    return this.mStraightBackgroundPool;
  }

  /**
   * PixiRenderer
   *
   * resolution and pixel ratio
   * ==========================
   * On windows devices the device pixel ratio will correspond to the
   * display settings scale, that can be different per screen
   * app.view will have the real device resolution
   * app.screen will have the desired resolution (inverse scaled)
   * (view.height = screen.height * device_scale * browser_scale)
   */
  constructor(params: IPixiParams = defaultConfig) {
    const pixelRatio = window.devicePixelRatio || 1;
    const screenWidth = params.width;
    const screenHeight = params.height;

    PIXI.settings.RESOLUTION = window.devicePixelRatio;
    PIXI.settings.PRECISION_FRAGMENT = PIXI.PRECISION.HIGH;
    PIXI.settings.MIPMAP_TEXTURES = PIXI.MIPMAP_MODES.ON;

    PIXI.utils.skipHello();

    this.app = new PIXI.Application({
      width: screenWidth,
      height: screenHeight,
      resolution: pixelRatio,
      antialias: true,
      backgroundAlpha: 1.0,
      backgroundColor: rendererDefaults.canvasBackgroundColor,
    });

    this.rootMaxBounds.set(
      this.app.screen.width * 0.5,
      this.app.screen.height * 0.5
    );
    this.rootMinBounds.set(
      -this.app.screen.width * 0.5,
      -this.app.screen.height * 0.5
    );

    window.addEventListener('resize', this.callWindowResize);

    // Global uniforms for shaders
    (this.app.renderer as PIXI.Renderer).globalUniforms.add(
      'CodemapGlobals',
      this.globalUniforms
    );
  }

  release(): void {
    if (this.mParentDivResizeObserver && this.mParentDivElement) {
      this.mParentDivResizeObserver.unobserve(this.mParentDivElement);
    }
    this.mParentDivResizeObserver = null;
    this.mParentDivElement = null;

    this.mShapesPool?.release();
    this.mShapesPool = null;

    this.mLinesPool?.release();
    this.mLinesPool = null;

    this.mCurvedBackgroundPool?.release();
    this.mCurvedBackgroundPool = null;

    this.mStraightBackgroundPool?.release();
    this.mStraightBackgroundPool = null;

    if (this.decalGeometry) {
      this.decalGeometry.destroy();
      this.decalGeometry = null;
    }

    this.mPrimitiveFactory.release();

    this.fontProgram = null;
    this.curvedBackgroundProgram = null;
    this.straightBackgroundProgram = null;
    this.lineProgram = null;
    this.sdfShapeProgram = null;
    this.instFontProgram = null;
    this.instCurvedBackgroundProgram = null;
    this.instStraightBackgroundProgram = null;
    this.instLineProgram = null;
    this.instSdfShapeProgram = null;

    this.releaseCriticalResources();
    this.releaseShaders();

    if (this.root) {
      this.root.core.children.forEach((child) => {
        child.destroy();
      });

      this.app.stage.removeChild(this.root.core);

      this.root.release();
      this.root = null;
    }

    this.parentDivElement = null;

    if (!this.bReleased) {
      window.removeEventListener('resize', this.callWindowResize);
      this.callWindowResize = () => {};

      this.onResize.empty();
      this.onLoadCriticalsCallbacks.empty();
      this.mTickDelegate = undefined;

      if (this.mCanvasWrapper) {
        this.mCanvasWrapper.removeChild(this.app.view);
        this.mCanvasWrapper = undefined;
      }

      const r = this.app.renderer;
      this.app.destroy(true, {
        children: true,
        texture: true,
        baseTexture: true,
      });

      cleanPixiRendererMemory(r);
    }

    PIXI.utils.destroyTextureCache();
    this.bReleased = true;
  }

  areCriticalsLoaded(): boolean {
    return this.bCriticalsLoaded && this.bShadersLoaded;
  }

  doWhenCriticalResourcesLoad(callback: QueuedCallback<IRenderer>): void {
    if (this.areCriticalsLoaded()) {
      callback(this);
    } else {
      this.onLoadCriticalsCallbacks.add(callback);
    }
  }

  private mCanvasWrapper: HTMLDivElement | undefined;

  async start(
    canvasWrapper: HTMLDivElement | null,
    parentDivElement: HTMLDivElement | null
  ): Promise<void> {
    // Abort (example change of web app page)?
    if (this.bReleased) {
      return;
    }

    try {
      if (canvasWrapper) {
        this.mCanvasWrapper = canvasWrapper;
        this.mCanvasWrapper.appendChild(this.app.view);
      }

      this.parentDivElement = parentDivElement;
      this.setupRoot();

      // Start loading criticals and shaders in parallel and wait for them afterwards
      const criticalsPromise = this.loadCriticalResources();
      const shadersPromise = this.loadShaders();

      await Promise.all([criticalsPromise, shadersPromise]);

      // After the async wait, there could be an abort
      if (this.bReleased) {
        this.release();
        return;
      }

      this.onLoadCriticalsCallbacks.call(this);

      await this.waitForEnablingTicking();

      if (this.bReleased) {
        this.release();
        return;
      }

      this.startTicking();
    } catch (e) {
      // eslint-disable-next-line no-console
      console.error(`Error in starting pixi renderer: ${e}`);
      this.release();
      throw e;
    }
  }

  async waitForEnablingTicking(): Promise<void> {
    while (!this.mTickDelegate && !this.bReleased) {
      // eslint-disable-next-line no-await-in-loop
      await new Promise((r) => setTimeout(r, 50));
    }
  }

  setTickDelegate(tickDelegate: ITickDelegate): void {
    this.mTickDelegate = tickDelegate;
  }

  // Load bitmap fonts and textures
  private async loadCriticalResources(): Promise<void> {
    if (this.bCriticalsLoaded || this.bReleased) {
      return;
    }

    const fontsList: { name: string; fontFile: string }[] = [];
    const imagesList: { name: string; bitmapFile: string }[] = [];
    const resourcesList: string[] = [];

    try {
      rendererDefaults.fonts.forEach((font) => {
        fontsList.push(font);
        resourcesList.push(font.fontFile);
      });

      rendererDefaults.images.forEach((image) => {
        imagesList.push(image);
        resourcesList.push(image.bitmapFile);
      });

      if (resourcesList.length > 0) {
        const assets = await this.assetLoader.load(resourcesList);

        if (this.bReleased) {
          this.releaseCriticalResources();
          return;
        }

        // Save references to fonts
        fontsList.forEach((font) => {
          this.setFont(font.name, assets[font.fontFile]);
        });

        // Save references to images
        imagesList.forEach((image) => {
          this.setImage(image.name, assets[image.bitmapFile]);
        });
      }

      this.bCriticalsLoaded = true;
    } catch (e) {
      if (!(e instanceof CanceledOperation)) {
        this.releaseCriticalResources();
        throw e;
      }
    }
  }

  private setFont(name: string, obj: unknown): void {
    if (obj instanceof PIXI.BitmapFont) {
      this.fontsMap.set(name, new PixiBitmapFont(name, obj));
    } else {
      throw new Error(`Invalid bitmap font object: ${name}`);
    }
  }

  private setImage(name: string, obj: unknown): void {
    if (obj instanceof PIXI.Texture) {
      this.bitmaps.set(name, obj);
    } else {
      throw new Error(`Invalid texture object: ${name}`);
    }
  }

  private releaseCriticalResources(): void {
    this.assetLoader.unloadAll();
    this.fontsMap.clear();
    this.bitmaps.clear();
  }

  private async loadShaders(): Promise<void> {
    if (this.bShadersLoaded || this.bReleased) {
      return;
    }

    try {
      await this.shaderMap.loadAll([
        // Font shaders
        rendererDefaults.shaders.instancedBitmapFont.vertPath,
        rendererDefaults.shaders.bitmapFont.fragPath,

        // Text background shaders
        rendererDefaults.shaders.instancedCurvedBackground.vertPath,
        rendererDefaults.shaders.curvedBackground.fragPath,
        rendererDefaults.shaders.instancedStraightBackground.vertPath,
        rendererDefaults.shaders.straightBackground.fragPath,

        // Line shaders
        rendererDefaults.shaders.instancedLine.vertPath,
        rendererDefaults.shaders.line.fragPath,

        // Shape shaders
        rendererDefaults.shaders.instancedSdfShapes.vertPath,
        rendererDefaults.shaders.instancedSdfShapes.fragPath,
      ]);

      if (this.bReleased) {
        this.releaseShaders();
        return;
      }

      let factoryType = PixiFactoryType.InstancedSDF;

      // Try using instanced SDF
      try {
        this.createPixiInstancedGraphics();
      } catch (instSdfError) {
        // eslint-disable-next-line no-console
        console.warn(
          `Unable to create instanced SDF graphics: ${instSdfError}`
        );

        // Non-instanced shaders will be necessary
        await this.shaderMap.loadAll([
          rendererDefaults.shaders.bitmapFont.vertPath,
          rendererDefaults.shaders.curvedBackground.vertPath,
          rendererDefaults.shaders.line.vertPath,
          rendererDefaults.shaders.sdfShapes.vertPath,
          rendererDefaults.shaders.sdfShapes.fragPath,
        ]);

        if (this.bReleased) {
          this.releaseShaders();
          return;
        }

        // Try non-instanced SDF
        try {
          this.createSDFShapeShader();
          this.createLineShader();
          this.createFontShader();
          this.createCurvedBackgroundShader();
          this.createStraightBackgroundShader();

          factoryType = PixiFactoryType.SDF;
        } catch (sdfError) {
          // eslint-disable-next-line no-console
          console.warn(`Unable to create SDF graphics: ${sdfError}`);

          // Go with conventional then
          factoryType = PixiFactoryType.Conventional;
        }
      }

      this.createPrimitiveFactory(factoryType);

      this.bShadersLoaded = true;
    } catch (e) {
      this.releaseShaders();
      throw e;
    }
  }

  private releaseShaders(): void {
    this.shaderMap.release();
  }

  private startTicking() {
    this.app.ticker.add(() => {
      if (this.bResize) {
        this.onWindowResize();
        this.bResize = false;
      }
      const dt = this.app.ticker.elapsedMS / 1000;
      this.accumulatedTickTime += dt;
      if (this.accumulatedTickTime >= this.minimumTickLenght) {
        this.globalUniforms.deltaTime = dt;
        this.globalUniforms.totalTime = this.totalTimeSeconds;
        this.globalUniforms.screenWidth = window.screen.width;
        this.globalUniforms.screenHeight = window.screen.height;
        this.globalUniforms.screenPixelRatio = this.pixelRatio;
        this.globalUniforms.screenScale = this.currentZoomFactor;

        if (this.mParentDivElement) {
          this.globalUniforms.viewportWidth =
            this.mParentDivElement.clientWidth * this.pixelRatio;
          this.globalUniforms.viewportHeight =
            this.mParentDivElement.clientHeight * this.pixelRatio;
        } else {
          this.globalUniforms.viewportWidth =
            window.screen.width * this.pixelRatio;
          this.globalUniforms.viewportHeight =
            window.screen.height * this.pixelRatio;
        }

        // Update instance graphics uniforms
        if (this.mShapesPool) {
          // Calculate the factor so the lines have always a certain weight in different resolutions
          const selectedSideLenght = Math.min(
            window.screen.width,
            window.screen.height
          );
          let strokeFactor = 1.0;
          if (selectedSideLenght > 2000.0) {
            strokeFactor = lerp(
              1.0,
              2.5,
              (selectedSideLenght - 2000.0) / 6000.0
            );
          }
          strokeFactor /= this.pixelRatio;

          this.mShapesPool.updateUniforms(strokeFactor);
        }

        this.mLinesPool?.updateUniforms(this.accumulatedTickTime);

        if (this.mTickDelegate) {
          this.mTickDelegate(this.accumulatedTickTime, this.totalTimeSeconds);
        }

        this.totalTimeSeconds += this.accumulatedTickTime;
        this.accumulatedTickTime = 0;
      }
    });
  }

  setTickMinimumLenght(value: number): void {
    this.minimumTickLenght = value;
  }

  onWindowResize(): void {
    if (this.bReleased) {
      return;
    }

    if (!this.mParentDivElement) {
      return;
    }

    this.onResize.call();

    const pixelRatio = window.devicePixelRatio || 1;
    const screenWidth = this.mParentDivElement.clientWidth / pixelRatio;
    const screenHeight = this.mParentDivElement.clientHeight / pixelRatio;

    this.app.renderer.resolution = pixelRatio;
    this.app.renderer.resize(screenWidth, screenHeight);
  }

  get pixelRatio(): number {
    return this.app.renderer.resolution;
  }

  testIfVisible(primitive: IPrimitive): CullStatus {
    if (this.root != null && this.root.core !== null) {
      const primitivePosition = primitive.getPosition();
      const x =
        primitivePosition.x * this.currentZoomFactor +
        this.root.core.position.x;

      const y =
        primitivePosition.y * this.currentZoomFactor +
        this.root.core.position.y;

      const r = primitive.getRadius() * this.currentZoomFactor;

      // For the tests we consider the circle as a bounding box
      // it would be more precise as a circle, but more expensiv

      // 1. Completly outside case
      if (
        x - r > this.screenWidth ||
        x + r < 0 ||
        y + r < 0 ||
        y - r > this.screenHeight
      ) {
        return CullStatus.Culled;
      }

      // 2.a bbox is completly inside screen
      if (
        x + r < this.screenWidth &&
        x - r > 0 &&
        y + r < this.screenHeight &&
        y - r > 0
      ) {
        return CullStatus.Visible;
      }
    }

    return CullStatus.Partially;
  }

  // Transforms from view coordinates (start from top left of view area, in view resolution),
  // to screen coordinates (similar )
  viewToScreen(viewToScreen: ScreenVector): ScreenVector {
    const hx = this.app.screen.width / this.app.view.width;
    const hy = this.app.screen.height / this.app.view.height;
    return new ScreenVector(viewToScreen.x * hx, viewToScreen.y * hy);
  }

  worldToScreen(world: ScreenVector): ScreenVector {
    if (this.root) {
      const screenCoordinates = this.root.core.worldTransform.apply(world);
      return new ScreenVector(screenCoordinates.x, screenCoordinates.y);
    }
    return new ScreenVector(0, 0);
  }

  // Transforms from screen coordinates to root coordinates
  screenToWorld(screen: ScreenVector): ScreenVector {
    if (this.root) {
      const worldCoordinates =
        this.root.core.worldTransform.applyInverse(screen);
      return new ScreenVector(worldCoordinates.x, worldCoordinates.y);
    }
    return new ScreenVector(0, 0);
  }

  private setupRoot() {
    this.root = new PixiRenderGroup(
      new ScreenVector(
        this.app.screen.width * 0.5,
        this.app.screen.height * 0.5
      )
    );

    if (this.root !== null) {
      this.root.core.sortableChildren = true;
      this.app.stage.addChild(this.root.core);
    }
  }

  private clampRootPosition() {
    if (this.root != null && this.root.core !== null) {
      this.root.core.position.x = Math.min(
        this.root.core.position.x,
        this.rootMaxBounds.x * this.root.getScale().x +
          this.app.screen.width * 0.5
      );
      this.root.core.position.y = Math.min(
        this.root.core.position.y,
        this.rootMaxBounds.y * this.root.getScale().y +
          this.app.screen.height * 0.5
      );

      this.root.core.position.x = Math.max(
        this.root.core.position.x,
        this.rootMinBounds.x * this.root.getScale().x +
          this.app.screen.width * 0.5
      );
      this.root.core.position.y = Math.max(
        this.root.core.position.y,
        this.rootMinBounds.y * this.root.getScale().y +
          this.app.screen.height * 0.5
      );
    }
  }

  testWorldPointInPrimitive(
    primitive: IPrimitive,
    worldPoint: ScreenVector
  ): boolean {
    if (!this.root) {
      return false;
    }

    const worldCenter = primitive.getPosition();
    const scaledRadius = primitive.getRadius();

    if (scaledRadius <= 0) {
      return false;
    }

    const distance2 = worldPoint.sqrDistanceTo(worldCenter);
    return distance2 < scaledRadius * scaledRadius;
  }

  testWorldCircleInPrimitive(
    primitive: IPrimitive,
    worldPoint: ScreenVector,
    radius: number
  ): boolean {
    if (!this.root) {
      return false;
    }

    const worldCenter = primitive.getPosition();
    const scaledRadius = primitive.getRadius();

    if (scaledRadius <= 0) {
      return false;
    }

    const dr = scaledRadius + radius;
    const dx = worldPoint.x - worldCenter.x;
    const dy = worldPoint.y - worldCenter.y;
    return dr > 0 && dr * dr > dx * dx + dy * dy;
  }

  getFont(fontName: string): PixiBitmapFont | undefined {
    return this.fontsMap.get(fontName);
  }

  getDecalGeometry(): PIXI.Geometry | null {
    return this.decalGeometry;
  }

  getTexture(bitmapName: string): PIXI.Texture<PIXI.Resource> | undefined {
    return this.bitmaps.get(bitmapName);
  }

  moveRoot(deltaPos: ScreenVector): void {
    if (this.root != null && this.root.core !== null) {
      this.root.core.position.x += deltaPos.x;
      this.root.core.position.y += deltaPos.y;

      this.clampRootPosition();
    }
  }

  moveRootToWorldPosition(localPos: ScreenVector): void {
    if (this.root !== null && this.root.core !== null) {
      const stagePosition = this.root.core.worldTransform.apply(localPos);
      this.root.core.position.x =
        this.app.screen.width * 0.5 - (stagePosition.x - this.root.core.x);
      this.root.core.position.y =
        this.app.screen.height * 0.5 - (stagePosition.y - this.root.core.y);
    }
  }

  setRootScale(value: number): void {
    if (this.root !== null && this.root.core !== null) {
      this.root.core.scale.x = value;
      this.root.core.scale.y = value;
    }
  }

  getRootGroupScale(): number {
    return this.root != null && this.root.core != null
      ? this.root.core.scale.x
      : 1.0;
  }

  get currentZoomFactor(): number {
    return this.getRootGroupScale();
  }

  getRootPosition(): ScreenVector {
    return this.root != null && this.root.core != null
      ? new ScreenVector(this.root.core.position.x, this.root.core.position.y)
      : new ScreenVector(0, 0);
  }

  setRootPosition(position: ScreenVector): void {
    if (this.root != null && this.root.core != null) {
      this.root.core.position.x = position.x;
      this.root.core.position.y = position.y;
    }
  }

  // Calculates the position in which the root has to be placed
  // so when is scaled to the corresponding zoom factor, it will
  // show the position centered on the screen
  calculateRootPositionToZoomTo(
    targetWorldPos: ScreenVector,
    zoomFactor: number
  ): ScreenVector {
    if (this.root != null && this.root.core != null) {
      // First calculate where we should place the root so the camera
      // is looking at he target position

      const newTransform = this.root.core.worldTransform.clone();
      const zoomRatio = zoomFactor / this.root.core.scale.x;
      const scaledTransform = newTransform.scale(zoomRatio, zoomRatio);

      const rootWorldPos = scaledTransform.apply(targetWorldPos);

      const oldPosition = new ScreenVector(
        this.app.screen.width * 0.5 -
          (rootWorldPos.x - this.root.core.x * zoomRatio),
        this.app.screen.height * 0.5 -
          (rootWorldPos.y - this.root.core.y * zoomRatio)
      );

      return new ScreenVector(oldPosition.x, oldPosition.y);
    }

    return new ScreenVector(0, 0);
  }

  getPositionToZoomIntoScreen(
    zoomValue: number,
    screenFocus: ScreenVector
  ): { position: ScreenVector; scale: ScreenVector } {
    if (this.root != null && this.root.core != null) {
      // Not only scale, but move the root group, so that
      // the current position under the pointer, is now
      // also the position under the mouse after applying the
      // new scale

      const localFocusPosition = this.app.stage.toLocal(
        new PIXI.Point(screenFocus.x, screenFocus.y)
      );

      const oldPosition = {
        x: this.root.core.position.x,
        y: this.root.core.position.y,
      };

      const worldPos = {
        x: (localFocusPosition.x - oldPosition.x) / this.root.core.scale.x,
        y: (localFocusPosition.y - oldPosition.y) / this.root.core.scale.y,
      };
      const newScale = { x: zoomValue, y: zoomValue };
      newScale.x = limitNumberTo(newScale.x, this.mMinZoom, this.mMaxZoom);
      newScale.y = limitNumberTo(newScale.y, this.mMinZoom, this.mMaxZoom);

      const newScreenPos = {
        x: worldPos.x * newScale.x + oldPosition.x,
        y: worldPos.y * newScale.y + oldPosition.y,
      };

      return {
        position: new ScreenVector(
          this.root.core.x - (newScreenPos.x - localFocusPosition.x),
          this.root.core.y - (newScreenPos.y - localFocusPosition.y)
        ),
        scale: new ScreenVector(newScale.x, newScale.y),
      };
    }

    return { position: ScreenVector.zero, scale: ScreenVector.zero };
  }

  zoomIntoScreenVector(delta: number, screenFocus: ScreenVector): void {
    if (this.root != null && this.root.core != null) {
      const zoomValue = this.root.core.scale.x - delta;
      const target = this.getPositionToZoomIntoScreen(zoomValue, screenFocus);

      this.root.core.x = target.position.x;
      this.root.core.y = target.position.y;
      this.root.core.scale.x = target.scale.x;
      this.root.core.scale.y = target.scale.y;

      this.clampRootPosition();
    }
  }

  private createPixiInstancedGraphics() {
    if (!this.root) {
      throw new Error('Null root container');
    }

    try {
      this.instSdfShapeProgram = this.shaderMap.createPixiProgram(
        rendererDefaults.shaders.instancedSdfShapes.name,
        rendererDefaults.shaders.instancedSdfShapes.vertPath,
        rendererDefaults.shaders.instancedSdfShapes.fragPath,
        true
      );

      this.instLineProgram = this.shaderMap.createPixiProgram(
        rendererDefaults.shaders.instancedLine.name,
        rendererDefaults.shaders.instancedLine.vertPath,
        rendererDefaults.shaders.line.fragPath,
        true
      );

      this.instFontProgram = this.shaderMap.createPixiProgram(
        rendererDefaults.shaders.instancedBitmapFont.name,
        rendererDefaults.shaders.instancedBitmapFont.vertPath,
        rendererDefaults.shaders.bitmapFont.fragPath,
        true
      );

      this.instCurvedBackgroundProgram = this.shaderMap.createPixiProgram(
        rendererDefaults.shaders.instancedCurvedBackground.name,
        rendererDefaults.shaders.instancedCurvedBackground.vertPath,
        rendererDefaults.shaders.curvedBackground.fragPath,
        true
      );

      this.instStraightBackgroundProgram = this.shaderMap.createPixiProgram(
        rendererDefaults.shaders.instancedStraightBackground.name,
        rendererDefaults.shaders.instancedStraightBackground.vertPath,
        rendererDefaults.shaders.straightBackground.fragPath,
        true
      );

      this.mShapesPool = new PixiShapesPool(
        100000,
        this.instSdfShapeProgram,
        this.root.core
      );

      this.mLinesPool = new PixiLinesPool(
        100000,
        this.instLineProgram,
        this.root.core
      );

      this.mCurvedBackgroundPool = new PixiCurvedBackgroundPool(
        10000,
        this.instCurvedBackgroundProgram,
        this.root.core
      );

      this.mStraightBackgroundPool = new PixiStraightBackgroundPool(
        10000,
        this.instStraightBackgroundProgram,
        this.root.core
      );
    } catch (e) {
      this.instSdfShapeProgram = null;
      this.instLineProgram = null;
      this.instFontProgram = null;
      this.instCurvedBackgroundProgram = null;
      this.instStraightBackgroundProgram = null;
      this.mShapesPool = null;
      this.mLinesPool = null;
      this.mCurvedBackgroundPool = null;
      this.mStraightBackgroundPool = null;
      throw e;
    }
  }

  private createSDFShapeShader() {
    this.sdfShapeProgram = this.shaderMap.createPixiProgram(
      rendererDefaults.shaders.sdfShapes.name,
      rendererDefaults.shaders.sdfShapes.vertPath,
      rendererDefaults.shaders.sdfShapes.fragPath
    );
    this.decalGeometry = new PIXI.Geometry()
      .addAttribute(
        'aVertexPosition', // the attribute name
        [
          -1.3,
          -1.3, // x, y
          1.3,
          -1.3, // x, y
          1.3,
          1.3,
          -1.3,
          1.3,
        ], // x, y
        2
      ) // the size of the attribute
      .addAttribute(
        'aUvs', // the attribute name
        [
          -1.3,
          -1.3, // u, v
          1.3,
          -1.3, // u, v
          1.3,
          1.3,
          -1.3,
          1.3,
        ], // u, v
        2
      ) // the size of the attribute
      .addIndex([0, 1, 2, 0, 2, 3]);
  }

  private createLineShader() {
    this.lineProgram = this.shaderMap.createPixiProgram(
      rendererDefaults.shaders.line.name,
      rendererDefaults.shaders.line.vertPath,
      rendererDefaults.shaders.line.fragPath
    );
  }

  private createFontShader() {
    this.fontProgram = this.shaderMap.createPixiProgram(
      rendererDefaults.shaders.bitmapFont.name,
      rendererDefaults.shaders.bitmapFont.vertPath,
      rendererDefaults.shaders.bitmapFont.fragPath
    );
  }

  private createCurvedBackgroundShader() {
    this.curvedBackgroundProgram = this.shaderMap.createPixiProgram(
      rendererDefaults.shaders.curvedBackground.name,
      rendererDefaults.shaders.curvedBackground.vertPath,
      rendererDefaults.shaders.curvedBackground.fragPath
    );
  }

  private createStraightBackgroundShader() {
    this.straightBackgroundProgram = this.shaderMap.createPixiProgram(
      rendererDefaults.shaders.straightBackground.name,
      rendererDefaults.shaders.curvedBackground.vertPath, // Same as curved version
      rendererDefaults.shaders.straightBackground.fragPath
    );
  }

  private createPrimitiveFactory(factoryType: PixiFactoryType) {
    switch (factoryType) {
      case PixiFactoryType.SDF:
        if (
          this.fontProgram &&
          this.curvedBackgroundProgram &&
          this.straightBackgroundProgram &&
          this.lineProgram &&
          this.decalGeometry &&
          this.sdfShapeProgram
        ) {
          this.mPrimitiveFactory = new PixiPrimitiveFactorySdf(
            this.bitmaps,
            this.fontsMap,
            this.fontProgram,
            this.lineProgram,
            this.decalGeometry,
            this.sdfShapeProgram,
            this.curvedBackgroundProgram,
            this.straightBackgroundProgram
          );
        }
        break;

      case PixiFactoryType.InstancedSDF:
        if (this.instFontProgram && this.root) {
          this.mPrimitiveFactory = new PixiPrimitiveFactoryInstanced(
            this.bitmaps,
            this.fontsMap,
            this.instFontProgram,
            this.root.core
          );
        }
        break;

      case PixiFactoryType.Mockup:
        this.mPrimitiveFactory = new MockPrimitiveFactory();
        break;

      default:
        if (this.fontProgram && this.lineProgram) {
          this.mPrimitiveFactory = new PixiPrimitiveFactory(
            this.bitmaps,
            this.fontsMap,
            this.fontProgram,
            this.lineProgram
          );
        }
        break;
    }
  }

  destroyPrimitive(primitive: IPrimitive): void {
    primitive.release();
  }

  addPrimitiveToRoot(primitive: IPrimitive): void {
    if (
      (primitive as PixiCorePrimitive) &&
      (primitive as PixiCorePrimitive).core
    )
      this.root!.core.addChild((primitive as PixiCorePrimitive).core);
  }
}
