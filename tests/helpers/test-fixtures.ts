import { DataModel, Enum, Model } from '@zenstackhq/sdk/ast'
import { BaseGeneratorContext } from '@core/types'
import { NormalizedOptions, validateOptions, PluginOptions } from '@utils/config'
import { OutputFormat } from '@utils/constants'
import type { PluginOptions as SdkPluginOptions } from '@zenstackhq/sdk'
import { getTestOutputPath } from '../test-setup'

export class TestFixtures {
	static createModel(models: DataModel[] = [], enums: Enum[] = []): Model {
		return {
			$type: 'Model',
			declarations: [...models, ...enums],
		} as Model
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
		const defaultAttributes = [TestFixtures.createAttribute('@graphql.filterable'), TestFixtures.createAttribute('@graphql.sortable')]
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
				type: undefined as any,
				optional: isOptional,
				array: isArray,
				reference: {
					ref: { name: referencedModel },
				},
			},
			attributes: [],
		}
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

	static createContext(options: Partial<PluginOptions & { models?: DataModel[]; enums?: Enum[] }> = {}): BaseGeneratorContext {
		const { models, enums, ...pluginOptions } = options
		return {
			options: TestFixtures.createOptions(pluginOptions),
			models: models || [],
			enums: enums || [],
		}
	}

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
			output: getTestOutputPath('schema.graphql'),
		}

		return { ...defaultOptions, ...overrides } as SdkPluginOptions
	}

	static createUserModel(withAttributes = true): DataModel {
		const fields = [TestFixtures.createField('id', 'String'), TestFixtures.createField('name', 'String'), TestFixtures.createField('email', 'String')]

		if (withAttributes) {
			fields.forEach((field) => {
				field.attributes = [TestFixtures.createAttribute('@graphql.filterable'), TestFixtures.createAttribute('@graphql.sortable')]
			})
		}

		return TestFixtures.createDataModel('User', fields)
	}

	static createPostModel(withRelations = true): DataModel {
		const fields = [
			TestFixtures.createField('id', 'String'),
			TestFixtures.createField('title', 'String'),
			TestFixtures.createField('content', 'String', true),
		]

		if (withRelations) {
			fields.push(TestFixtures.createRelationField('author', 'User') as any)
		}

		return TestFixtures.createDataModel('Post', fields)
	}

	static createUserRoleEnum(): Enum {
		return TestFixtures.createEnum('UserRole', ['ADMIN', 'USER', 'MODERATOR'])
	}

	static createPriorityEnum(): Enum {
		return TestFixtures.createEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])
	}

	static createBlogContext(): BaseGeneratorContext {
		return TestFixtures.createContext({
			models: [TestFixtures.createUserModel(), TestFixtures.createPostModel()],
			enums: [TestFixtures.createUserRoleEnum(), TestFixtures.createPriorityEnum()],
		})
	}

	static createMinimalContext(): BaseGeneratorContext {
		return TestFixtures.createContext({
			generateScalars: false,
			generateEnums: false,
			generateFilters: false,
			generateSorts: false,
			connectionTypes: false,
			includeRelations: false,
		})
	}

	static createScalarOnlyContext(): BaseGeneratorContext {
		return TestFixtures.createContext({
			generateScalars: true,
			generateEnums: false,
			generateFilters: false,
			generateSorts: false,
			connectionTypes: false,
			includeRelations: false,
			scalarTypes: {
				DateTime: 'DateTime',
				Json: 'JSON',
				Decimal: 'Decimal',
				Bytes: 'String',
			},
		})
	}
}

export class TestAssertions {
	static expectArrayContains<T>(actual: T[], expected: T, message?: string): void {
		if (!actual.includes(expected)) {
			const msg = message || `Expected array to contain ${JSON.stringify(expected)}`
			throw new Error(`${msg}\nActual: ${JSON.stringify(actual)}`)
		}
	}

	static expectArrayLength<T>(actual: T[], expectedLength: number, message?: string): void {
		if (actual.length !== expectedLength) {
			const msg = message || `Expected array length to be ${expectedLength}, but was ${actual.length}`
			throw new Error(`${msg}\nActual: ${JSON.stringify(actual)}`)
		}
	}

	static expectStringContains(actual: string, expected: string, message?: string): void {
		if (!actual.includes(expected)) {
			const msg = message || `Expected string to contain "${expected}"`
			throw new Error(`${msg}\nActual: "${actual}"`)
		}
	}

	static expectStringNotContains(actual: string, expected: string, message?: string): void {
		if (actual.includes(expected)) {
			const msg = message || `Expected string not to contain "${expected}"`
			throw new Error(`${msg}\nActual: "${actual}"`)
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

	static createMockAttribute(name: string, args: Array<{ name?: string; value: any }> = []) {
		return TestFixtures.createAttribute(name, args)
	}
}
