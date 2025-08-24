import { DataModel, Enum, Model } from '@zenstackhq/sdk/ast'
import { BaseGeneratorContext, GeneratorContext } from '@core/types'
import { NormalizedOptions, validateOptions, PluginOptions } from '@utils/config'
import { OutputFormat } from '@utils/constants'
import { UnifiedGeneratorContext, OutputStrategy } from '@generators/strategies'
import { SchemaComposer } from 'graphql-compose'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { GraphQLRegistry, TypeScriptRegistry } from '@utils/registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { MockOutputStrategy, SpyOutputStrategy } from './mock-strategy'
import type { PluginOptions as SdkPluginOptions } from '@zenstackhq/sdk'
import { getTestOutputPath } from '../test-setup'

export class TestBuilders {
	static createEnum(name: string, values: string[] = []): Enum {
		return {
			$type: 'Enum',
			name,
			fields: values.map((value) => ({
				$type: 'EnumField',
				name: value,
				attributes: [],
				$container: undefined as any,
				comments: [],
			})) as any,
			attributes: [],
			$container: undefined as any,
			comments: [],
		} as Enum
	}

	static createField(name: string, type: string, isOptional = false, attributes: any[] = []) {
		const defaultAttributes = [TestBuilders.createAttribute('@graphql.filterable'), TestBuilders.createAttribute('@graphql.sortable')]
		return {
			$type: 'DataModelField',
			name,
			type: {
				type,
				optional: isOptional,
				array: false,
				reference: null,
			},
			attributes: attributes.length > 0 ? attributes : defaultAttributes,
		}
	}

	static createRelationField(name: string, referencedModel: string, isOptional = false, isArray = false) {
		return {
			$type: 'DataModelField',
			name,
			type: {
				type: undefined,
				optional: isOptional,
				array: isArray,
				reference: {
					ref: { name: referencedModel },
				},
			},
			attributes: [],
		}
	}

	static createDataModel(name: string, fields: any[] = []): DataModel {
		return {
			$type: 'DataModel',
			name,
			fields,
			attributes: [],
			isAbstract: false,
			$container: undefined as any,
			comments: [],
			isView: false,
			superTypes: [],
		} as DataModel
	}

	static createModel(models: DataModel[] = [], enums: Enum[] = []): Model {
		return {
			$type: 'Model',
			declarations: [...models, ...enums],
		} as Model
	}

	static createAttribute(name: string, args: Array<{ name?: string; value: any }> = []) {
		return {
			$type: name.startsWith('@@') ? 'DataModelAttribute' : 'DataModelFieldAttribute',
			decl: {
				ref: { name },
			},
			args: args.map((arg) => ({
				name: arg.name,
				value: {
					$type:
						typeof arg.value === 'string'
							? 'StringLiteral'
							: typeof arg.value === 'number'
								? 'NumberLiteral'
								: typeof arg.value === 'boolean'
									? 'BooleanLiteral'
									: 'StringLiteral',
					value: arg.value,
				},
			})),
			$container: undefined as any,
		} as any
	}
}

export class ConfigurationBuilder {
	static createOptions(overrides: Partial<PluginOptions> = {}): NormalizedOptions {
		return validateOptions({
			output: getTestOutputPath('generated'),
			outputFormat: OutputFormat.GRAPHQL,
			generateScalars: true,
			generateEnums: true,
			generateFilters: true,
			generateSorts: true,
			connectionTypes: true,
			includeRelations: true,
			fieldNaming: 'camelCase',
			typeNaming: 'PascalCase',
			schemaPath: getTestOutputPath('schema.prisma'),
			...overrides,
		})
	}

	static createSdkOptions(overrides: Partial<SdkPluginOptions> = {}): SdkPluginOptions {
		const defaultOptions: SdkPluginOptions = {
			schemaPath: getTestOutputPath('schema.prisma'),
			provider: 'postgresql',
		}

		const result = { ...defaultOptions, ...overrides } as unknown
		return result as SdkPluginOptions
	}
}

export class ContextBuilder {
	static createBaseContext(options: Partial<PluginOptions & { models?: DataModel[]; enums?: Enum[] }> = {}): BaseGeneratorContext {
		const { models, enums, ...pluginOptions } = options
		return {
			options: ConfigurationBuilder.createOptions(pluginOptions),
			models: models || [],
			enums: enums || [],
		}
	}

	static createGraphQLContext(context: BaseGeneratorContext = ContextBuilder.createBaseContext()): GeneratorContext {
		const schemaComposer = new SchemaComposer()
		const typeFormatter = TypeFormatter.fromOptions(context.options.typeNaming, context.options.fieldNaming)

		return {
			...context,
			schemaComposer,
			attributeProcessor: new SchemaProcessor(),
			registry: new GraphQLRegistry(schemaComposer),
			typeFormatter,
			typeMapper: new UnifiedTypeMapper(typeFormatter, context.models, context.enums, context.options),
			typeFactories: new GraphQLTypeFactories(schemaComposer, typeFormatter),
		}
	}

	static createUnifiedContext(context: BaseGeneratorContext = ContextBuilder.createBaseContext()): UnifiedGeneratorContext {
		return {
			outputStrategy: new MockOutputStrategy(),
			options: context.options,
			models: context.models,
			enums: context.enums,
			typeFormatter: TypeFormatter.fromOptions(context.options.typeNaming, context.options.fieldNaming),
			attributeProcessor: new SchemaProcessor(),
			typeMapper: new UnifiedTypeMapper(
				TypeFormatter.fromOptions(context.options.typeNaming, context.options.fieldNaming),
				context.models,
				context.enums,
				context.options,
			),
		}
	}

	static createSpyUnifiedContext(context: BaseGeneratorContext = ContextBuilder.createBaseContext()): UnifiedGeneratorContext & { spy: SpyOutputStrategy } {
		const spy = new SpyOutputStrategy()
		return {
			outputStrategy: spy,
			options: context.options,
			models: context.models,
			enums: context.enums,
			typeFormatter: TypeFormatter.fromOptions(context.options.typeNaming, context.options.fieldNaming),
			attributeProcessor: new SchemaProcessor(),
			typeMapper: new UnifiedTypeMapper(
				TypeFormatter.fromOptions(context.options.typeNaming, context.options.fieldNaming),
				context.models,
				context.enums,
				context.options,
			),
			spy,
		}
	}
}

export class TestAssertions {
	static expectArrayContains<T>(actual: T[], expected: T): void {
		if (!actual.includes(expected)) {
			throw new Error(`Expected array to contain ${expected}, but it didn't. Actual: ${JSON.stringify(actual)}`)
		}
	}

	static expectArrayLength<T>(actual: T[], expectedLength: number): void {
		if (actual.length !== expectedLength) {
			throw new Error(`Expected array length to be ${expectedLength}, but was ${actual.length}. Actual: ${JSON.stringify(actual)}`)
		}
	}

	static expectStringContains(actual: string, expected: string): void {
		if (!actual.includes(expected)) {
			throw new Error(`Expected string to contain "${expected}", but it didn't. Actual: "${actual}"`)
		}
	}

	static expectStringNotContains(actual: string, expected: string): void {
		if (actual.includes(expected)) {
			throw new Error(`Expected string not to contain "${expected}", but it did. Actual: "${actual}"`)
		}
	}
}

export class TestUtilities {
	static mockFunction<T extends (...args: any[]) => any>(implementation?: T): T {
		const fn = implementation || (() => {})
		return fn as T
	}

	static delay(ms: number): Promise<void> {
		return new Promise((resolve) => setTimeout(resolve, ms))
	}

	static randomString(length = 10): string {
		return Math.random()
			.toString(36)
			.substring(2, length + 2)
	}

	static randomInt(min = 0, max = 100): number {
		return Math.floor(Math.random() * (max - min + 1)) + min
	}
}
