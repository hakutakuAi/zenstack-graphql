import { describe, it, expect } from 'bun:test'
import { TestFixtures } from '../../helpers'
import { OutputFormat } from '@utils/constants'

describe('TestFixtures', () => {
	describe('Basic Creation Methods', () => {
		it('should create a basic data model', () => {
			const model = TestFixtures.createDataModel('User', [])

			expect(model.name).toBe('User')
			expect(model.$type).toBe('DataModel')
			expect(model.fields).toEqual([])
			expect(model.attributes).toEqual([])
			expect(model.isAbstract).toBe(false)
		})

		it('should create an enum with values', () => {
			const enumType = TestFixtures.createEnum('Status', ['ACTIVE', 'INACTIVE'])

			expect(enumType.name).toBe('Status')
			expect(enumType.$type).toBe('Enum')
			expect(enumType.fields).toHaveLength(2)
			expect(enumType.fields[0].name).toBe('ACTIVE')
			expect(enumType.fields[1].name).toBe('INACTIVE')
		})

		it('should create a field with correct structure', () => {
			const field = TestFixtures.createField('name', 'String', false, false)

			expect(field.$type).toBe('DataModelField')
			expect(field.name).toBe('name')
			expect(field.type.type).toBe('String')
			expect(field.type.optional).toBe(false)
			expect(field.type.array).toBe(false)
			expect(field.attributes).toHaveLength(2)
		})

		it('should create relation field correctly', () => {
			const field = TestFixtures.createRelationField('user', 'User', true, false)

			expect(field.$type).toBe('DataModelField')
			expect(field.name).toBe('user')
			expect(field.type.optional).toBe(true)
			expect(field.type.array).toBe(false)
			expect(field.type.reference.ref.name).toBe('User')
		})

		it('should create attributes correctly', () => {
			const attribute = TestFixtures.createAttribute('@id')

			expect(attribute.$type).toBe('DataModelFieldAttribute')
			expect(attribute.decl.ref.name).toBe('@id')
			expect(attribute.args).toEqual([])
		})

		it('should create attributes with arguments', () => {
			const attribute = TestFixtures.createAttribute('@db.VarChar', [{ value: 255 }])

			expect(attribute.args).toHaveLength(1)
			expect(attribute.args[0].value.value).toBe(255)
			expect(attribute.args[0].value.$type).toBe('NumberLiteral')
		})
	})

	describe('Field Variations', () => {
		it('should create optional fields', () => {
			const field = TestFixtures.createField('description', 'String', true)

			expect(field.type.optional).toBe(true)
			expect(field.type.array).toBe(false)
		})

		it('should create array fields', () => {
			const field = TestFixtures.createField('tags', 'String', false, true)

			expect(field.type.optional).toBe(false)
			expect(field.type.array).toBe(true)
		})

		it('should create optional array fields', () => {
			const field = TestFixtures.createField('categories', 'String', true, true)

			expect(field.type.optional).toBe(true)
			expect(field.type.array).toBe(true)
		})

		it('should create fields with custom attributes', () => {
			const customAttr = TestFixtures.createAttribute('@unique')
			const field = TestFixtures.createField('email', 'String', false, false, [customAttr])

			expect(field.attributes).toHaveLength(1)
			expect(field.attributes[0]).toBe(customAttr)
		})
	})

	describe('Context Creation', () => {
		it('should create default context', () => {
			const context = TestFixtures.createContext()

			expect(context.options).toBeDefined()
			expect(context.models).toEqual([])
			expect(context.enums).toEqual([])
		})

		it('should create context with models', () => {
			const userModel = TestFixtures.createDataModel('User')
			const context = TestFixtures.createContext({ models: [userModel] })

			expect(context.models).toHaveLength(1)
			expect(context.models[0]).toBe(userModel)
		})

		it('should create context with enums', () => {
			const statusEnum = TestFixtures.createEnum('Status', ['ACTIVE'])
			const context = TestFixtures.createContext({ enums: [statusEnum] })

			expect(context.enums).toHaveLength(1)
			expect(context.enums[0]).toBe(statusEnum)
		})

		it('should create context with custom options', () => {
			const context = TestFixtures.createContext({
				outputFormat: OutputFormat.TYPE_GRAPHQL,
				generateFilters: false,
				generateSorts: false,
			})

			expect(context.options.outputFormat).toBe(OutputFormat.TYPE_GRAPHQL)
			expect(context.options.generateFilters).toBe(false)
			expect(context.options.generateSorts).toBe(false)
		})
	})

	describe('Pre-built Models', () => {
		it('should create user model', () => {
			const model = TestFixtures.createUserModel()

			expect(model.name).toBe('User')
			expect(model.fields).toHaveLength(3)

			const fieldNames = model.fields.map((f: any) => f.name)
			expect(fieldNames).toContain('id')
			expect(fieldNames).toContain('name')
			expect(fieldNames).toContain('email')
		})

		it('should create post model with relations', () => {
			const model = TestFixtures.createPostModel()

			expect(model.name).toBe('Post')
			expect(model.fields).toHaveLength(4)

			const fieldNames = model.fields.map((f: any) => f.name)
			expect(fieldNames).toContain('author')
		})

		it('should create post model without relations', () => {
			const model = TestFixtures.createPostModel(false)

			expect(model.name).toBe('Post')
			expect(model.fields).toHaveLength(3)

			const fieldNames = model.fields.map((f: any) => f.name)
			expect(fieldNames).not.toContain('author')
		})

		it('should create all field types model', () => {
			const model = TestFixtures.createAllFieldTypesModel()

			expect(model.name).toBe('AllTypes')
			expect(model.fields.length).toBeGreaterThan(10)

			const fieldTypes = model.fields.map((f: any) => f.type.type)
			expect(fieldTypes).toContain('String')
			expect(fieldTypes).toContain('Int')
			expect(fieldTypes).toContain('Float')
			expect(fieldTypes).toContain('Boolean')
			expect(fieldTypes).toContain('DateTime')
			expect(fieldTypes).toContain('Json')
			expect(fieldTypes).toContain('Decimal')
		})
	})

	describe('Pre-built Contexts', () => {
		it('should create blog context', () => {
			const context = TestFixtures.createBlogContext()

			expect(context.models).toHaveLength(2)
			expect(context.enums).toHaveLength(2)

			const modelNames = context.models.map((m) => m.name)
			expect(modelNames).toContain('User')
			expect(modelNames).toContain('Post')

			const enumNames = context.enums.map((e) => e.name)
			expect(enumNames).toContain('UserRole')
			expect(enumNames).toContain('Priority')
		})

		it('should create e-commerce context', () => {
			const context = TestFixtures.createECommerceContext()

			expect(context.models).toHaveLength(3)
			expect(context.enums).toHaveLength(1)

			const modelNames = context.models.map((m) => m.name)
			expect(modelNames).toContain('User')
			expect(modelNames).toContain('Product')
			expect(modelNames).toContain('Order')

			expect(context.enums[0].name).toBe('OrderStatus')
		})

		it('should create complex relation context', () => {
			const context = TestFixtures.createComplexRelationContext()

			expect(context.models).toHaveLength(3)
			expect(context.options.includeRelations).toBe(true)

			const modelNames = context.models.map((m) => m.name)
			expect(modelNames).toContain('Organization')
			expect(modelNames).toContain('Department')
			expect(modelNames).toContain('Employee')
		})

		it('should create minimal context', () => {
			const context = TestFixtures.createMinimalContext()

			expect(context.options.generateScalars).toBe(false)
			expect(context.options.generateEnums).toBe(false)
			expect(context.options.generateFilters).toBe(false)
			expect(context.options.generateSorts).toBe(false)
			expect(context.options.connectionTypes).toBe(false)
			expect(context.options.includeRelations).toBe(false)
		})
	})

	describe('Enum Creation', () => {
		it('should create user role enum', () => {
			const enumType = TestFixtures.createUserRoleEnum()

			expect(enumType.name).toBe('UserRole')
			expect(enumType.fields).toHaveLength(3)

			const values = enumType.fields.map((f: any) => f.name)
			expect(values).toContain('ADMIN')
			expect(values).toContain('USER')
			expect(values).toContain('MODERATOR')
		})

		it('should create priority enum', () => {
			const enumType = TestFixtures.createPriorityEnum()

			expect(enumType.name).toBe('Priority')
			expect(enumType.fields).toHaveLength(3)

			const values = enumType.fields.map((f: any) => f.name)
			expect(values).toContain('HIGH')
			expect(values).toContain('MEDIUM')
			expect(values).toContain('LOW')
		})

		it('should create empty enum', () => {
			const enumType = TestFixtures.createEnum('Empty', [])

			expect(enumType.name).toBe('Empty')
			expect(enumType.fields).toHaveLength(0)
		})
	})

	describe('Model Creation', () => {
		it('should create model from models and enums', () => {
			const userModel = TestFixtures.createDataModel('User')
			const statusEnum = TestFixtures.createEnum('Status', ['ACTIVE'])
			const model = TestFixtures.createModel([userModel], [statusEnum])

			expect(model.$type).toBe('Model')
			expect(model.declarations).toHaveLength(2)
			expect(model.declarations[0]).toBe(userModel)
			expect(model.declarations[1]).toBe(statusEnum)
		})

		it('should create empty model', () => {
			const model = TestFixtures.createModel()

			expect(model.$type).toBe('Model')
			expect(model.declarations).toHaveLength(0)
		})
	})
})
