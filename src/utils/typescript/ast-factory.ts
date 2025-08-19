import { Project, SourceFile, ClassDeclaration, EnumDeclaration } from 'ts-morph'
import { DataModel, DataModelField, Enum } from '@zenstackhq/sdk/ast'
import { TypeFormatter } from '@utils/schema/type-formatter'

export class TypeScriptASTFactory {
	private project: Project
	private sourceFile: SourceFile

	constructor(private readonly typeFormatter: TypeFormatter) {
		this.project = new Project()
		this.sourceFile = this.project.createSourceFile('generated.ts')
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
		const typeName = this.typeFormatter.formatTypeName(model.name)

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

		const validFields = model.fields.filter((field) => field.type.type)

		for (const field of validFields) {
			this.addFieldToClass(classDeclaration, field)
		}

		return classDeclaration
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

	createScalarType(name: string, nativeType: string): string {
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
