import { Project, SourceFile, ClassDeclaration, EnumDeclaration } from 'ts-morph'
import { DataModel, DataModelField, Enum, isDataModel } from '@zenstackhq/sdk/ast'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { SchemaProcessor } from '@utils/schema/schema-processor'

export class TypeScriptASTFactory {
	private project: Project
	private sourceFile: SourceFile
	private typeMapper?: UnifiedTypeMapper
	private schemaProcessor?: SchemaProcessor

	constructor(
		private readonly typeFormatter: TypeFormatter,
		typeMapper?: UnifiedTypeMapper,
		schemaProcessor?: SchemaProcessor,
	) {
		this.project = new Project()
		this.sourceFile = this.project.createSourceFile('generated.ts')
		this.typeMapper = typeMapper
		this.schemaProcessor = schemaProcessor
		this.addImports()
	}

	private addImports(): void {
		this.sourceFile.addImportDeclarations([
			{
				moduleSpecifier: 'type-graphql',
				namedImports: ['ObjectType', 'Field', 'ID', 'Int', 'Float', 'registerEnumType', 'InputType', 'ArgsType'],
			},
			{
				moduleSpecifier: 'graphql-scalars',
				namedImports: ['GraphQLJSON'],
			},
			{
				moduleSpecifier: 'reflect-metadata',
				defaultImport: undefined,
			},
		])
	}

	createObjectType(model: DataModel): ClassDeclaration {
		const typeName = this.schemaProcessor
			? this.schemaProcessor.model(model).getFormattedTypeName(this.typeFormatter)
			: this.typeFormatter.formatTypeName(model.name)

		const classDeclaration = this.sourceFile.addClass({
			name: typeName,
			isExported: true,
			decorators: [
				{
					name: 'ObjectType',
					arguments: [],
				},
			],
		})

		const validFields = model.fields.filter((field) => {
			return field.type.type || this.typeMapper?.isRelationField(field)
		})

		for (const field of validFields) {
			this.addFieldToClass(classDeclaration, field)
		}

		return classDeclaration
	}

	createObjectTypeFromFields(typeName: string, fields: Record<string, any>, description?: string): ClassDeclaration {
		const classDeclaration = this.sourceFile.addClass({
			name: typeName,
			isExported: true,
			decorators: [
				{
					name: 'ObjectType',
					arguments: description ? [`{ description: "${description}" }`] : [],
				},
			],
		})

		for (const [fieldName, fieldConfig] of Object.entries(fields)) {
			const graphqlType = fieldConfig.type
			const tsType = this.mapGraphQLTypeToTS(graphqlType)
			const isRequired = graphqlType.includes('!')
			const isNullable = !isRequired

			classDeclaration.addProperty({
				name: fieldName,
				type: tsType,
				hasQuestionToken: isNullable,
				hasExclamationToken: isRequired,
				decorators: [
					{
						name: 'Field',
						arguments: this.getSimpleFieldDecoratorArgs(graphqlType, isNullable),
					},
				],
			})
		}

		return classDeclaration
	}

	private getSimpleFieldDecoratorArgs(graphqlType: string, isNullable: boolean): string[] {
		const cleanType = graphqlType.replace(/[\[\]!]/g, '')
		const isArray = graphqlType.includes('[')

		let decoratorType = cleanType
		if (cleanType === 'DateTime') {
			decoratorType = 'Date'
		}

		if (isArray) {
			decoratorType = `[${decoratorType}]`
		}

		const args = [`() => ${decoratorType}`]
		if (isNullable) {
			args.push('{ nullable: true }')
		}

		return args
	}

	private mapGraphQLTypeToTS(graphqlType: string): string {
		const cleanType = graphqlType.replace(/[\[\]!]/g, '')
		const isArray = graphqlType.includes('[')

		let tsType = 'any'
		switch (cleanType) {
			case 'String':
				tsType = 'string'
				break
			case 'Int':
			case 'Float':
				tsType = 'number'
				break
			case 'Boolean':
				tsType = 'boolean'
				break
			case 'DateTime':
			case 'Date':
				tsType = 'Date'
				break
			case 'ID':
				tsType = 'string'
				break
			case 'JSON':
				tsType = 'any'
				break
			default:
				tsType = cleanType
		}

		return isArray ? `${tsType}[]` : tsType
	}

	createEnumType(enumType: Enum): EnumDeclaration {
		const typeName = this.typeFormatter.formatTypeName(enumType.name)

		const enumDeclaration = this.sourceFile.addEnum({
			name: typeName,
			isExported: true,
			members: enumType.fields.map((field) => ({
				name: field.name,
				value: field.name,
			})),
		})

		this.sourceFile.addStatements(`
registerEnumType(${typeName}, {
  name: '${typeName}',
})`)

		return enumDeclaration
	}

	createInputType(model: DataModel, suffix: string): ClassDeclaration {
		const typeName = this.typeFormatter.formatTypeName(`${model.name}${suffix}`)

		const classDeclaration = this.sourceFile.addClass({
			name: typeName,
			isExported: true,
			decorators: [
				{
					name: 'InputType',
					arguments: [],
				},
			],
		})

		const validFields = model.fields.filter((field) => field.type.type)

		for (const field of validFields) {
			this.addInputFieldToClass(classDeclaration, field)
		}

		return classDeclaration
	}

	private addFieldToClass(classDeclaration: ClassDeclaration, field: DataModelField): void {
		const fieldName = this.typeFormatter.formatFieldName(field.name)
		const fieldType = this.getFieldType(field)
		const typeScriptType = this.getTypeScriptType(field)

		const decoratorArgs = this.getFieldDecoratorArgs(field, fieldType)

		classDeclaration.addProperty({
			name: fieldName,
			type: typeScriptType,
			hasQuestionToken: field.type.optional,
			hasExclamationToken: !field.type.optional,
			decorators: [
				{
					name: 'Field',
					arguments: decoratorArgs,
				},
			],
		})
	}

	private addInputFieldToClass(classDeclaration: ClassDeclaration, field: DataModelField): void {
		const fieldName = this.typeFormatter.formatFieldName(field.name)
		const fieldType = this.getFieldType(field)
		const typeScriptType = this.getTypeScriptType(field)

		const decoratorArgs = this.getFieldDecoratorArgs(field, fieldType)

		classDeclaration.addProperty({
			name: fieldName,
			type: typeScriptType,
			hasQuestionToken: field.type.optional,
			hasExclamationToken: !field.type.optional,
			decorators: [
				{
					name: 'Field',
					arguments: decoratorArgs,
				},
			],
		})
	}

	private getFieldDecoratorArgs(field: DataModelField, fieldType: string): string[] {
		const args = [`() => ${fieldType}`]

		if (field.type.optional) {
			args.push('{ nullable: true }')
		}

		return args
	}

	private getFieldType(field: DataModelField): string {
		if (this.typeMapper?.isRelationField(field)) {
			const referencedModel = field.type.reference?.ref
			if (referencedModel && this.schemaProcessor && isDataModel(referencedModel)) {
				const relationTypeName = this.schemaProcessor.model(referencedModel).getFormattedTypeName(this.typeFormatter)
				if (field.type.array) {
					return `[${relationTypeName}]`
				}
				return relationTypeName
			} else {
				const relationTypeName = this.typeFormatter.formatTypeName(field.type.reference?.ref?.name || 'String')
				if (field.type.array) {
					return `[${relationTypeName}]`
				}
				return relationTypeName
			}
		}

		if (field.type.array) {
			return `[${this.getScalarFieldType(field)}]`
		}
		return this.getScalarFieldType(field)
	}

	private getScalarFieldType(field: DataModelField): string {
		switch (field.type.type) {
			case 'String':
				return 'String'
			case 'Int':
			case 'BigInt':
				return 'Int'
			case 'Float':
				return 'Float'
			case 'Boolean':
				return 'Boolean'
			case 'DateTime':
				return 'Date'
			case 'Json':
				return 'GraphQLJSON'
			case 'Decimal':
				return 'Float'
			case 'Bytes':
				return 'String'
			default:
				return this.typeFormatter.formatTypeName(field.type.type || 'String')
		}
	}

	private getTypeScriptType(field: DataModelField): string {
		if (this.typeMapper?.isRelationField(field)) {
			const referencedModel = field.type.reference?.ref
			if (referencedModel && this.schemaProcessor && isDataModel(referencedModel)) {
				const relationTypeName = this.schemaProcessor.model(referencedModel).getFormattedTypeName(this.typeFormatter)
				if (field.type.array) {
					return `${relationTypeName}[]`
				}
				return relationTypeName
			} else {
				const relationTypeName = this.typeFormatter.formatTypeName(field.type.reference?.ref?.name || 'string')
				if (field.type.array) {
					return `${relationTypeName}[]`
				}
				return relationTypeName
			}
		}

		const baseType = this.getTypeScriptBaseType(field)
		if (field.type.array) {
			return `${baseType}[]`
		}
		return baseType
	}

	private getTypeScriptBaseType(field: DataModelField): string {
		switch (field.type.type) {
			case 'String':
			case 'Bytes':
				return 'string'
			case 'Int':
			case 'BigInt':
			case 'Float':
			case 'Decimal':
				return 'number'
			case 'Boolean':
				return 'boolean'
			case 'DateTime':
				return 'Date'
			case 'Json':
				return 'any'
			default:
				return this.typeFormatter.formatTypeName(field.type.type || 'string')
		}
	}

	createSortDirectionEnum(): EnumDeclaration {
		const enumDeclaration = this.sourceFile.addEnum({
			name: 'SortDirection',
			isExported: true,
			members: [
				{ name: 'ASC', value: 'ASC' },
				{ name: 'DESC', value: 'DESC' },
			],
		})

		this.sourceFile.addStatements(`
registerEnumType(SortDirection, {
  name: 'SortDirection',
  description: 'Sort direction for ordering results',
})`)

		return enumDeclaration
	}

	createFilterInputType(name: string, fields: Array<{ name: string; type: string; nullable?: boolean }>): ClassDeclaration {
		const classDeclaration = this.sourceFile.addClass({
			name,
			isExported: true,
			decorators: [
				{
					name: 'InputType',
					arguments: [],
				},
			],
		})

		for (const field of fields) {
			const decoratorArgs = [`() => ${field.type}`]
			if (field.nullable) {
				decoratorArgs.push('{ nullable: true }')
			}

			let tsType = this.convertGraphQLTypeToTypeScript(field.type)
			if (field.nullable) {
				tsType = `${tsType} | undefined`
			}

			classDeclaration.addProperty({
				name: field.name,
				type: tsType,
				hasQuestionToken: field.nullable,
				hasExclamationToken: !field.nullable,
				decorators: [
					{
						name: 'Field',
						arguments: decoratorArgs,
					},
				],
			})
		}

		return classDeclaration
	}

	private convertGraphQLTypeToTypeScript(graphqlType: string): string {
		if (graphqlType.startsWith('[') && graphqlType.endsWith(']')) {
			const innerType = graphqlType.slice(1, -1).replace('!', '')
			return `${this.convertGraphQLTypeToTypeScript(innerType)}[]`
		}

		switch (graphqlType.replace('!', '')) {
			case 'String':
				return 'string'
			case 'Float':
			case 'Int':
				return 'number'
			case 'Boolean':
				return 'boolean'
			case 'Date':
				return 'Date'
			default:
				return graphqlType.replace('!', '')
		}
	}

	createSortInputType(modelName: string, fields: Array<{ name: string; description?: string }>): ClassDeclaration {
		const sortInputName = this.typeFormatter.formatTypeName(`${modelName}SortInput`)

		const classDeclaration = this.sourceFile.addClass({
			name: sortInputName,
			isExported: true,
			decorators: [
				{
					name: 'InputType',
					arguments: [],
				},
			],
		})

		for (const field of fields) {
			classDeclaration.addProperty({
				name: field.name,
				type: 'SortDirection | undefined',
				hasQuestionToken: true,
				decorators: [
					{
						name: 'Field',
						arguments: ['() => SortDirection', '{ nullable: true }'],
					},
				],
			})
		}

		return classDeclaration
	}

	private pageInfoCreated = false

	createPageInfo(): void {
		if (this.pageInfoCreated) {
			return
		}
		this.pageInfoCreated = true

		const pageInfoDeclaration = this.sourceFile.addClass({
			name: 'PageInfo',
			isExported: true,
			decorators: [
				{
					name: 'ObjectType',
					arguments: [],
				},
			],
		})

		const pageInfoFields = [
			{ name: 'hasNextPage', type: 'Boolean', tsType: 'boolean' },
			{ name: 'hasPreviousPage', type: 'Boolean', tsType: 'boolean' },
			{ name: 'startCursor', type: 'String', tsType: 'string', nullable: true },
			{ name: 'endCursor', type: 'String', tsType: 'string', nullable: true },
		]

		for (const field of pageInfoFields) {
			const decoratorArgs = [`() => ${field.type}`]
			if (field.nullable) {
				decoratorArgs.push('{ nullable: true }')
			}

			pageInfoDeclaration.addProperty({
				name: field.name,
				type: field.nullable ? `${field.tsType} | undefined` : field.tsType,
				hasQuestionToken: !!field.nullable,
				hasExclamationToken: !field.nullable,
				decorators: [
					{
						name: 'Field',
						arguments: decoratorArgs,
					},
				],
			})
		}
	}

	createConnectionType(modelName: string): { edge: ClassDeclaration; connection: ClassDeclaration } {
		this.createPageInfo()

		const typeName = this.typeFormatter.formatTypeName(modelName)
		const edgeName = `${typeName}Edge`
		const connectionName = `${typeName}Connection`

		const edgeDeclaration = this.sourceFile.addClass({
			name: edgeName,
			isExported: true,
			decorators: [
				{
					name: 'ObjectType',
					arguments: [],
				},
			],
		})

		edgeDeclaration.addProperty({
			name: 'node',
			type: typeName,
			hasExclamationToken: true,
			decorators: [
				{
					name: 'Field',
					arguments: [`() => ${typeName}`],
				},
			],
		})

		edgeDeclaration.addProperty({
			name: 'cursor',
			type: 'string',
			hasExclamationToken: true,
			decorators: [
				{
					name: 'Field',
					arguments: ['() => String'],
				},
			],
		})

		const connectionDeclaration = this.sourceFile.addClass({
			name: connectionName,
			isExported: true,
			decorators: [
				{
					name: 'ObjectType',
					arguments: [],
				},
			],
		})

		connectionDeclaration.addProperty({
			name: 'pageInfo',
			type: 'PageInfo',
			hasExclamationToken: true,
			decorators: [
				{
					name: 'Field',
					arguments: ['() => PageInfo'],
				},
			],
		})

		connectionDeclaration.addProperty({
			name: 'edges',
			type: `${edgeName}[]`,
			hasExclamationToken: true,
			decorators: [
				{
					name: 'Field',
					arguments: [`() => [${edgeName}]`],
				},
			],
		})

		connectionDeclaration.addProperty({
			name: 'totalCount',
			type: 'number',
			hasExclamationToken: true,
			decorators: [
				{
					name: 'Field',
					arguments: ['() => Int'],
				},
			],
		})

		return {
			edge: edgeDeclaration,
			connection: connectionDeclaration,
		}
	}

	createPaginationInputTypes(): ClassDeclaration[] {
		const forwardPagination = this.sourceFile.addClass({
			name: 'ForwardPaginationInput',
			isExported: true,
			decorators: [
				{
					name: 'InputType',
					arguments: [],
				},
			],
		})

		forwardPagination.addProperty({
			name: 'first',
			type: 'number | undefined',
			hasQuestionToken: true,
			decorators: [
				{
					name: 'Field',
					arguments: ['() => Int', '{ nullable: true }'],
				},
			],
		})

		forwardPagination.addProperty({
			name: 'after',
			type: 'string | undefined',
			hasQuestionToken: true,
			decorators: [
				{
					name: 'Field',
					arguments: ['() => String', '{ nullable: true }'],
				},
			],
		})

		const backwardPagination = this.sourceFile.addClass({
			name: 'BackwardPaginationInput',
			isExported: true,
			decorators: [
				{
					name: 'InputType',
					arguments: [],
				},
			],
		})

		backwardPagination.addProperty({
			name: 'last',
			type: 'number | undefined',
			hasQuestionToken: true,
			decorators: [
				{
					name: 'Field',
					arguments: ['() => Int', '{ nullable: true }'],
				},
			],
		})

		backwardPagination.addProperty({
			name: 'before',
			type: 'string | undefined',
			hasQuestionToken: true,
			decorators: [
				{
					name: 'Field',
					arguments: ['() => String', '{ nullable: true }'],
				},
			],
		})

		const combinedPagination = this.sourceFile.addClass({
			name: 'PaginationInput',
			isExported: true,
			decorators: [
				{
					name: 'InputType',
					arguments: [],
				},
			],
		})

		const paginationFields = [
			{ name: 'first', type: 'Int', tsType: 'number' },
			{ name: 'after', type: 'String', tsType: 'string' },
			{ name: 'last', type: 'Int', tsType: 'number' },
			{ name: 'before', type: 'String', tsType: 'string' },
		]

		for (const field of paginationFields) {
			combinedPagination.addProperty({
				name: field.name,
				type: `${field.tsType} | undefined`,
				hasQuestionToken: true,
				decorators: [
					{
						name: 'Field',
						arguments: [`() => ${field.type}`, '{ nullable: true }'],
					},
				],
			})
		}

		return [forwardPagination, backwardPagination, combinedPagination]
	}

	createScalarType(name: string, _nativeType: string): string {
		return `export const ${name}Scalar = new GraphQLScalarType({
  name: '${name}',
  description: '${name} scalar type',
  serialize: (value: any) => value,
  parseValue: (value: any) => value,
  parseLiteral: (ast: any) => ast.value,
})`
	}

	getGeneratedCode(): string {
		return this.sourceFile.getFullText()
	}

	clear(): void {
		this.sourceFile.removeText()
		this.addImports()
	}
}
