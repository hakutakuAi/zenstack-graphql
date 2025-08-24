import { DataModel, Enum, Model } from '@zenstackhq/sdk/ast'
import { BaseGeneratorContext } from '@core/types'
import { NormalizedOptions, validateOptions, PluginOptions } from '@utils/config'
import { OutputFormat } from '@utils/constants'
import type { PluginOptions as SdkPluginOptions } from '@zenstackhq/sdk'
import { getTestOutputPath } from '../test-setup'

export class TestUtils {
	static createMockModel(models: DataModel[] = [], enums: Enum[] = []): Model {
		return {
			$type: 'Model',
			declarations: [...models, ...enums],
		} as Model
	}

	static createMockDataModel(name: string, fields: any[] = []): DataModel {
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

	static createMockEnum(name: string, values: string[] = []): Enum {
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

	static createMockField(name: string, type: string, isOptional = false) {
		return {
			$type: 'DataModelField',
			name,
			type: {
				type,
				optional: isOptional,
				array: false,
				reference: null,
			},
			attributes: [],
		}
	}

	static createMockRelationField(name: string, referencedModel: string, isOptional = false, isArray = false) {
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

	static createMockContext(options: Partial<PluginOptions & { models?: DataModel[]; enums?: Enum[] }> = {}): BaseGeneratorContext {
		const { models, enums, ...pluginOptions } = options
		return {
			options: this.createMockOptions(pluginOptions),
			models: models || [],
			enums: enums || [],
		}
	}

	static createMockOptions(overrides: Partial<PluginOptions> = {}): NormalizedOptions {
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

	static createMockSdkOptions(overrides: Partial<SdkPluginOptions> = {}): SdkPluginOptions {
		const defaultOptions: SdkPluginOptions = {
			schemaPath: getTestOutputPath('schema.prisma'),
			provider: 'postgresql',
			output: getTestOutputPath('schema.graphql'),
		}

		const result = { ...defaultOptions, ...overrides } as unknown
		return result as SdkPluginOptions
	}

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

	static mockFunction<T extends (...args: any[]) => any>(implementation?: T): T {
		const fn = implementation || (() => {})
		return fn as T
	}

	static createMockAttribute(name: string, args: Array<{ name?: string; value: any }> = []) {
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
