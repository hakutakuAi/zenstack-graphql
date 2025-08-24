import { DataModel, Enum, Model } from '@zenstackhq/sdk/ast'
import { TestFixtures } from './test-fixtures'

export class SchemaBuilder {
	private models: DataModel[] = []
	private enums: Enum[] = []

	addModel(name: string): ModelBuilder {
		const modelBuilder = new ModelBuilder(name, this)
		modelBuilder.onBuild((model) => {
			this.models.push(model)
		})
		return modelBuilder
	}

	addEnum(name: string, values: string[] = []): SchemaBuilder {
		this.enums.push(TestFixtures.createEnum(name, values))
		return this
	}

	build(): Model {
		return TestFixtures.createModel(this.models, this.enums)
	}

	getModels(): DataModel[] {
		return this.models
	}

	getEnums(): Enum[] {
		return this.enums
	}

	static create(): SchemaBuilder {
		return new SchemaBuilder()
	}

	static createSimpleUser(): SchemaBuilder {
		return SchemaBuilder.create()
			.addModel('User')
			.addField('id', 'String')
			.addField('name', 'String')
			.addField('email', 'String')
			.addField('age', 'Int', true)
			.addField('active', 'Boolean')
			.addField('createdAt', 'DateTime')
			.finish()
	}

	static createBlogSchema(): SchemaBuilder {
		return SchemaBuilder.create()
			.addEnum('UserRole', ['ADMIN', 'USER', 'MODERATOR'])
			.addEnum('PostStatus', ['DRAFT', 'PUBLISHED', 'ARCHIVED'])
			.addModel('User')
			.addField('id', 'String')
			.addField('name', 'String')
			.addField('email', 'String')
			.addField('role', 'UserRole')
			.addField('active', 'Boolean')
			.addField('createdAt', 'DateTime')
			.addRelation('posts', 'Post', false, true)
			.finish()
			.addModel('Post')
			.addField('id', 'String')
			.addField('title', 'String')
			.addField('content', 'String', true)
			.addField('status', 'PostStatus')
			.addField('publishedAt', 'DateTime', true)
			.addField('createdAt', 'DateTime')
			.addRelation('author', 'User')
			.addRelation('comments', 'Comment', false, true)
			.finish()
			.addModel('Comment')
			.addField('id', 'String')
			.addField('text', 'String')
			.addField('createdAt', 'DateTime')
			.addRelation('post', 'Post')
			.addRelation('author', 'User')
			.finish()
	}

	static createECommerceSchema(): SchemaBuilder {
		return SchemaBuilder.create()
			.addEnum('OrderStatus', ['PENDING', 'PROCESSING', 'SHIPPED', 'DELIVERED', 'CANCELLED'])
			.addEnum('ProductCategory', ['ELECTRONICS', 'CLOTHING', 'BOOKS', 'HOME', 'SPORTS'])
			.addModel('User')
			.addField('id', 'String')
			.addField('name', 'String')
			.addField('email', 'String')
			.addField('phone', 'String', true)
			.addField('createdAt', 'DateTime')
			.addRelation('orders', 'Order', false, true)
			.finish()
			.addModel('Product')
			.addField('id', 'String')
			.addField('name', 'String')
			.addField('description', 'String', true)
			.addField('price', 'Decimal')
			.addField('stock', 'Int')
			.addField('category', 'ProductCategory')
			.addField('active', 'Boolean')
			.addField('createdAt', 'DateTime')
			.addRelation('orderItems', 'OrderItem', false, true)
			.finish()
			.addModel('Order')
			.addField('id', 'String')
			.addField('total', 'Decimal')
			.addField('status', 'OrderStatus')
			.addField('createdAt', 'DateTime')
			.addRelation('user', 'User')
			.addRelation('items', 'OrderItem', false, true)
			.finish()
			.addModel('OrderItem')
			.addField('id', 'String')
			.addField('quantity', 'Int')
			.addField('price', 'Decimal')
			.addRelation('order', 'Order')
			.addRelation('product', 'Product')
			.finish()
	}
}

export class ModelBuilder {
	private fields: any[] = []
	private attributes: any[] = []
	private buildCallback?: (model: DataModel) => void
	private schemaBuilder?: SchemaBuilder

	constructor(
		private name: string,
		schemaBuilder?: SchemaBuilder,
	) {
		this.schemaBuilder = schemaBuilder
	}

	addField(name: string, type: string, optional = false): ModelBuilder {
		this.fields.push(TestFixtures.createField(name, type, optional))
		return this
	}

	addRelation(name: string, referencedModel: string, optional = false, isArray = false): ModelBuilder {
		this.fields.push(TestFixtures.createRelationField(name, referencedModel, optional, isArray))
		return this
	}

	addAttribute(name: string, args: Array<{ name?: string; value: any }> = []): ModelBuilder {
		this.attributes.push(TestFixtures.createAttribute(name, args))
		return this
	}

	addIgnoreAttribute(): ModelBuilder {
		return this.addAttribute('@@graphql.ignore')
	}

	addNameAttribute(customName: string): ModelBuilder {
		return this.addAttribute('@@graphql.name', [{ value: customName }])
	}

	addDescriptionAttribute(description: string): ModelBuilder {
		return this.addAttribute('@@graphql.description', [{ value: description }])
	}

	onBuild(callback: (model: DataModel) => void): ModelBuilder {
		this.buildCallback = callback
		return this
	}

	finish(): SchemaBuilder {
		const model = this.build()
		if (this.buildCallback) {
			this.buildCallback(model)
		}
		return this.schemaBuilder || new SchemaBuilder()
	}

	private build(): DataModel {
		return {
			...TestFixtures.createDataModel(this.name, this.fields),
			attributes: this.attributes,
		}
	}
}

export class FieldBuilder {
	constructor(
		private name: string,
		private type: string,
	) {}

	optional(): FieldBuilder {
		return this
	}

	array(): FieldBuilder {
		return this
	}

	addAttribute(name: string, args: any[] = []): FieldBuilder {
		return this
	}

	addIgnore(): FieldBuilder {
		return this.addAttribute('@graphql.ignore')
	}

	addSortable(): FieldBuilder {
		return this.addAttribute('@graphql.sortable')
	}

	addFilterable(): FieldBuilder {
		return this.addAttribute('@graphql.filterable')
	}

	addCustomName(name: string): FieldBuilder {
		return this.addAttribute('@graphql.name', [name])
	}

	build() {
		return TestFixtures.createField(this.name, this.type)
	}
}
